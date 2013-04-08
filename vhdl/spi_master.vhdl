--
-- Copyright (C) 2011, 2013 Chris McClelland
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_master is
	generic (
		FAST_COUNT : unsigned(5 downto 0) := "000000"; -- maxcount for fast mode: defaults to sysClk/2 (24MHz @48MHz)
		SLOW_COUNT : unsigned(5 downto 0) := "111011"  -- maxcount for slow mode: defaults to sysClk/120 (400kHz @48MHz)
	);
	port(
		reset_in     : in  std_logic;
		clk_in       : in  std_logic;
		turbo_in     : in  std_logic;

		-- Client interface
		sendData_in   : in  std_logic_vector(7 downto 0);
		sendValid_in  : in  std_logic;
		sendReady_out : out std_logic;

		recvData_out  : out std_logic_vector(7 downto 0);
		recvValid_out : out std_logic;
		recvReady_in  : in  std_logic;

		-- SPI interface
		spiClk_out  : out std_logic;
		spiData_out : out std_logic;
		spiData_in  : in  std_logic
	);
end entity;
 
architecture rtl of spi_master is
	-- Sender FSM
	type SSType is (
		SS_IDLE,
		SS_RECV_COUNT,
		SS_RECV_NOCOUNT,
		SS_SCLK_LOW,       -- drive LSB on spiData whilst holding spiClk low
		SS_SCLK_HIGH       -- drive LSB on spiData whilst holding spiClk high
	);
	signal state, state_next            : SSType := SS_IDLE;
	signal shiftOut, shiftOut_next      : std_logic_vector(7 downto 0) := (others => '0');  -- outbound shift reg
	signal shiftIn, shiftIn_next        : std_logic_vector(6 downto 0) := (others => '0');  -- inbound shift reg
	signal recvData, recvData_next      : std_logic_vector(7 downto 0) := (others => '0');  -- receive side dbl.buf
	signal cycleCount, cycleCount_next  : unsigned(5 downto 0) := (others => '0');          -- num cycles per 1/2 bit
	signal cycleCount_init              : unsigned(5 downto 0) := (others => '0');          --
	signal bitCount, bitCount_next      : unsigned(2 downto 0) := (others => '0');          -- num bits remaining
begin
	-- Infer registers
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			if ( reset_in = '1' ) then
				state <= SS_IDLE;
				shiftOut <= (others => '0');
				shiftIn <= (others => '0');
				recvData <= (others => '0');
				bitCount <= "000";
				cycleCount <= (others => '0');
			else
				state <= state_next;
				shiftOut <= shiftOut_next;
				shiftIn <= shiftIn_next;
				recvData <= recvData_next;
				bitCount <= bitCount_next;
				cycleCount <= cycleCount_next;
			end if;
		end if;
	end process;

	cycleCount_init <=
		FAST_COUNT when turbo_in = '1' else
		SLOW_COUNT;

	-- Next state logic
	process(state, sendData_in, sendValid_in, recvData, recvReady_in,
			  shiftOut, shiftIn, spiData_in,
			  cycleCount_init, cycleCount, bitCount)
	begin
		state_next <= state;
		shiftOut_next  <= shiftOut;
		shiftIn_next <= shiftIn;
		recvData_next <= recvData;
		recvValid_out <= '0';
		cycleCount_next <= cycleCount;
		bitCount_next <= bitCount;
		spiData_out <= shiftOut(0);  -- always drive the LSB on spiData_out
		sendReady_out <= '0';  -- not ready for data
		case state is
			-- Wait for data on the send pipe for us to clock out
			when SS_IDLE =>
				spiClk_out  <= '1';
				sendReady_out <= '1';  -- ready for data
				if ( sendValid_in = '1' ) then
					-- we've got some data to send...prepare to clock it out
					state_next <= SS_SCLK_LOW;           -- spiClk going low
					shiftOut_next <= sendData_in;        -- get send byte from FIFO
					bitCount_next <= "111";              -- need eight bits for a full byte
					cycleCount_next <= cycleCount_init;  -- initialise the delay counter
				end if;

			-- Drive bit on spiData, and hold spiClk low for cycleCount+1 cycles
			when SS_SCLK_LOW =>
				spiClk_out <= '0';
				cycleCount_next <= cycleCount - 1;
				if ( cycleCount = 0 ) then
					-- Time to move on to SS_SCLK_HIGH - prepare to sample input data
					state_next <= SS_SCLK_HIGH;
					shiftIn_next <= spiData_in & shiftIn(6 downto 1);
					cycleCount_next <= cycleCount_init;
					if ( bitCount = 0 ) then
						-- Update the recvData register
						recvData_next <= spiData_in & shiftIn(6 downto 0);
						state_next <= SS_RECV_COUNT;
					end if;
				end if;

			-- Wait for consumption of received byte whilst counting down
			when SS_RECV_COUNT =>
				spiClk_out <= '1';
				recvValid_out <= '1';
				cycleCount_next <= cycleCount - 1;
				if ( cycleCount = 0 ) then
					state_next <= SS_RECV_NOCOUNT;
				end if;
				if ( recvReady_in = '1' ) then
					if ( cycleCount = 0 ) then
						-- Received byte will be consumed, but we're out of time - time to begin new byte if necessary
						sendReady_out <= '1';
						if ( sendValid_in = '1' ) then
							-- There's a new byte to send
							state_next <= SS_SCLK_LOW;
							shiftOut_next <= sendData_in;
							bitCount_next <= "111";
							cycleCount_next <= cycleCount_init;
						else
							-- Nothing to do, go idle
							state_next <= SS_IDLE;
						end if;
					else
						-- Received byte will be consumed - go to HIGH state for remainder of count
						state_next <= SS_SCLK_HIGH;
					end if;
				else
					if ( cycleCount = 0 ) then
						-- Received byte will not be consumed yet, and we're out of time - wait in NOCOUNT
						state_next <= SS_RECV_NOCOUNT;
					else
						-- Received byte will not be consumed yet, but we're not yet out of time - wait here
					end if;
				end if;

			-- Wait for consumption of received byte; no countdown
			when SS_RECV_NOCOUNT =>
				spiClk_out <= '1';
				recvValid_out <= '1';
				if ( recvReady_in = '1' ) then
					-- Received byte will be consumed - time to begin new byte if necessary
					sendReady_out <= '1';
					if ( sendValid_in = '1' ) then
						-- There's a new byte to send
						state_next <= SS_SCLK_LOW;
						shiftOut_next <= sendData_in;
						bitCount_next <= "111";
						cycleCount_next <= cycleCount_init;
					else
						-- Nothing to do, go idle
						state_next <= SS_IDLE;
					end if;
				end if;

			-- Carry on driving bit on spiData, hold spiClk high for four cycles
			when SS_SCLK_HIGH =>
				spiClk_out <= '1';
				cycleCount_next <= cycleCount - 1;
				if ( cycleCount = 0 ) then
					-- Time to move back to SS_SCLK_LOW or SS_IDLE - shift next bit out on next clock edge
					shiftOut_next <= "0" & shiftOut(7 downto 1);
					bitCount_next <= bitCount - 1;
					cycleCount_next <= cycleCount_init;
					if ( bitCount = 0 ) then
						-- This was the last bit...see if there's another byte to send or go back to idle state
						sendReady_out <= '1';
						if ( sendValid_in = '1' ) then
							-- There's a new byte to send
							state_next <= SS_SCLK_LOW;
							shiftOut_next <= sendData_in;
							bitCount_next <= "111";
							cycleCount_next <= cycleCount_init;
						else
							-- Nothing to do, go idle
							state_next <= SS_IDLE;
						end if;
					else
						-- This was not the last bit...do another clock
						state_next <= SS_SCLK_LOW;
					end if;
				end if;
		end case;
	end process;

	recvData_out <= recvData;

end architecture;

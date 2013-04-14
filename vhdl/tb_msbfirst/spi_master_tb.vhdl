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
use ieee.std_logic_textio.all;
use std.textio.all;
use work.hex_util.all;

entity spi_master_tb is
end entity;

architecture behavioural of spi_master_tb is
	-- Clocks, etc
	signal sysClk     : std_logic;  -- main system clock
	signal dispClk    : std_logic;  -- display version of sysClk, which transitions 4ns before it
	signal reset      : std_logic;

	-- Client interface
	signal sendData   : std_logic_vector(7 downto 0);  -- data to send
	signal sendValid  : std_logic;
	signal sendReady  : std_logic;
	signal recvData   : std_logic_vector(7 downto 0);  -- data we receive
	signal recvValid  : std_logic;
	signal recvReady  : std_logic;

	-- External interface
	signal spiClk     : std_logic;  -- serial clock
	signal spiDataOut : std_logic;  -- send serial data
	signal spiDataIn  : std_logic;  -- receive serial data
begin
	-- Instantiate the unit under test
	uut: entity work.spi_master
		generic map(
			--FAST_COUNT => "000011"
			FAST_COUNT => "000000",
			BIT_ORDER  => '1'
		)
		port map(
			reset_in      => reset,
			clk_in        => sysClk,
			
			turbo_in      => '1',
			suppress_in   => '0',
			sendData_in   => sendData,
			sendValid_in  => sendValid,
			sendReady_out => sendReady,
			
			recvData_out  => recvData,
			recvValid_out => recvValid,
			recvReady_in  => recvReady,
			
			spiClk_out    => spiClk,
			spiData_out   => spiDataOut,
			spiData_in    => spiDataIn
		);

	-- Drive the clocks. In simulation, sysClk lags 4ns behind dispClk, to give a visual hold time
	-- for signals in GTKWave.
	process
	begin
		sysClk <= '0';
		dispClk <= '0';
		wait for 16 ns;
		loop
			dispClk <= not(dispClk);  -- first dispClk transitions
			wait for 4 ns;
			sysClk <= not(sysClk);  -- then sysClk transitions, 4ns later
			wait for 6 ns;
		end loop;
	end process;

	-- Deassert the synchronous reset a couple of cycles after startup.
	--
	process
	begin
		reset <= '1';
		wait until rising_edge(sysClk);
		wait until rising_edge(sysClk);
		reset <= '0';
		wait;
	end process;

	-- Drive the unit under test. Read stimulus from stimulus.sim and write results to results.sim
	process
		variable inLine  : line;
		variable outLine : line;
		file inFile      : text open read_mode is "stimulus/send.sim";
		file outFile     : text open write_mode is "results/recv.sim";
	begin
		sendData <= (others => 'X');
		sendValid <= '0';
		wait until falling_edge(reset);
		wait until rising_edge(sysClk);
		while ( not endfile(inFile) ) loop
			readline(inFile, inLine);
			while ( inLine.all'length = 0 or inLine.all(1) = '#' or inLine.all(1) = ht or inLine.all(1) = ' ' ) loop
				readline(inFile, inLine);
			end loop;
			sendData <= to_4(inLine.all(1)) & to_4(inLine.all(2));
			sendValid <= to_1(inLine.all(4));
			recvReady <= to_1(inLine.all(6));
			wait for 10 ns;
			write(outLine, from_4(sendData(7 downto 4)) & from_4(sendData(3 downto 0)));
			write(outLine, ' ');
			write(outLine, sendValid);
			write(outLine, ' ');
			write(outLine, sendReady);
			writeline(outFile, outLine);
			wait for 10 ns;
		end loop;
		sendData <= (others => 'X');
		sendValid <= '0';
		wait;
	end process;

	-- Mock the serial interface's interlocutor: send from s/recv.sim and receive into r/send.sim
	process
		variable inLine, outLine : line;
		variable inData, outData : std_logic_vector(7 downto 0);
		file inFile              : text open read_mode is "stimulus/recv.sim";
		file outFile             : text open write_mode is "results/send.sim";
	begin
		spiDataIn <= 'X';
		loop
			exit when endfile(inFile);
			readline(inFile, inLine);
			read(inLine, inData);
			wait until spiClk = '0';
			spiDataIn <= inData(7);
			wait until spiClk = '1';
			outData(7) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(6);
			wait until spiClk = '1';
			outData(6) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(5);
			wait until spiClk = '1';
			outData(5) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(4);
			wait until spiClk = '1';
			outData(4) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(3);
			wait until spiClk = '1';
			outData(3) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(2);
			wait until spiClk = '1';
			outData(2) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(1);
			wait until spiClk = '1';
			outData(1) := spiDataOut;
			wait until spiClk = '0';
			spiDataIn <= inData(0);
			wait until spiClk = '1';
			outData(0) := spiDataOut;
			write(outLine, outData);
			writeline(outFile, outLine);
		end loop;
		wait for 10 ns;
		spiDataIn <= 'X';
		wait;
	end process;
end architecture;

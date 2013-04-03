--
-- Copyright (C) 2011, 2013 Chris McClelland
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;

entity serial_io_tb is
end serial_io_tb;

architecture behavioural of serial_io_tb is
	-- Clocks, etc
	signal sysClk   : std_logic;
	signal reset    : std_logic;

	-- Client interface
	signal sendData : std_logic_vector(7 downto 0);  -- data to send
	signal recvData : std_logic_vector(7 downto 0);  -- data we receive
	signal load     : std_logic;  -- begin a new send/receive operation
	signal busy     : std_logic;  -- flag to indicate when serial_io unit is busy sending/receiving

	-- External interface
	signal sDataOut : std_logic;  -- send serial data
	signal sDataIn  : std_logic;  -- receive serial data
	signal sClk     : std_logic;  -- serial clock
begin
	-- Instantiate the unit under test
	uut: entity work.serial_io
		port map(
			reset_in  => reset,
			clk_in    => sysClk,
			data_in   => sendData,
			data_out  => recvData,
			load_in   => load,
			turbo_in  => '1',
			busy_out  => busy,
			sData_out => sDataOut,
			sData_in  => sDataIn,
			sClk_out  => sClk
		);

	-- Drive the clock
	process
	begin
		sysClk <= '0';
		wait for 5 ns;
		sysClk <= '1';
		wait for 5 ns;
	end process;

	-- Drive the serial interface: send from s/send.sim and receive into r/recv.sim
	process
		variable inLine, outLine : line;
		variable inData, outData : std_logic_vector(7 downto 0);
		file inFile              : text open read_mode is "stimulus/send.sim";
		file outFile             : text open write_mode is "results/recv.sim";
	begin
		sendData <= (others => 'X');
		load <= '0';
		reset <= '1';
		wait for 10 ns;
		reset <= '0';
		wait for 40 ns;
		loop
			exit when endfile(inFile);
			readline(inFile, inLine);
			read(inLine, inData);
			sendData <= inData;
			load <= '1';
			wait for 10 ns;
			sendData <= (others => 'X');
			load <= '0';
			wait until busy = '0';
			outData := recvData;
			write(outLine, outData);
			writeline(outFile, outLine);
		end loop;
		wait;
		--assert false report "NONE. End of simulation." severity failure;
	end process;

	-- Mock the serial interface's interlocutor: send from s/recv.sim and receive into r/send.sim
	process
		variable inLine, outLine : line;
		variable inData, outData : std_logic_vector(7 downto 0);
		file inFile              : text open read_mode is "stimulus/recv.sim";
		file outFile             : text open write_mode is "results/send.sim";
	begin
		sDataIn <= 'X';
		loop
			exit when endfile(inFile);
			readline(inFile, inLine);
			read(inLine, inData);
			wait until sClk = '0';
			sDataIn <= inData(0);
			wait until sClk = '1';
			outData(0) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(1);
			wait until sClk = '1';
			outData(1) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(2);
			wait until sClk = '1';
			outData(2) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(3);
			wait until sClk = '1';
			outData(3) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(4);
			wait until sClk = '1';
			outData(4) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(5);
			wait until sClk = '1';
			outData(5) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(6);
			wait until sClk = '1';
			outData(6) := sDataOut;
			wait until sClk = '0';
			sDataIn <= inData(7);
			wait until sClk = '1';
			outData(7) := sDataOut;
			write(outLine, outData);
			writeline(outFile, outLine);
		end loop;
		wait for 10 ns;
		sDataIn <= 'X';
		wait;
	end process;
end architecture;

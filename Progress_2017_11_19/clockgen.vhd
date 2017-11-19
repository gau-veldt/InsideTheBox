----------------------------------------------------------------------------------
--
--  Clock generation logic
--
--  Generates a phase 0 clock and non-overlapping
-- phase 1 and phase 2 clocks from a 4X input clock source
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity clockgen is
port (
    ph4Xin  :   in  std_logic;
    ph0     :   out std_logic;
    ph1     :   out std_logic;
    ph2     :   out std_logic;
    stg     :   out std_logic_vector(1 downto 0);
    res0    :   in  std_logic
);
end clockgen;

architecture clock_impl of clockgen is

signal clkStg : std_logic_vector(1 downto 0) := "00";

begin
    count: process(ph4Xin,res0) is
    begin
        if (res0 = '0') then
            clkStg <= "01";
        elsif (rising_edge(ph4xin)) then
            clkStg <= ((clkStg(1) xor clkStg(0)) & (not clkStg(0)));
        end if;
    end process count;
    
    ph0 <= clkStg(1);
    ph1 <= clkStg(1) and (not clkStg(0));
    ph2 <= not (clkStg(1) or clkStg(0));
    stg <= clkStg;

end clock_impl;

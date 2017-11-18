----------------------------------------------------------------------------------
--
-- Clocking Example using an MMCM tile in the Zynq
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity clocking is port (
    led         : out std_logic_vector(3 downto 0);
    clk_125     : in std_logic
);
end clocking;

architecture clock_impl of clocking is

component clk_wiz_0
port
 (-- Clock in ports
  -- Clock out ports
  clk8M          : out    std_logic;
  -- Status and control signals
  reset             : in     std_logic;
  locked            : out    std_logic;
  clk_in1           : in     std_logic
 );
end component;

signal clk8M : std_logic;
signal reset : std_logic;
signal locked : std_logic;

subtype u24 is unsigned(23 downto 0);
signal count : u24 := x"000000";
constant count8m : u24 := to_unsigned(4000000,24);
signal slow_clock : std_logic := '0';

begin

clock : clk_wiz_0
   port map ( 
  -- Clock out ports  
   clk8M => clk8M,
  -- Status and control signals                
   reset => reset,
   locked => locked,
   -- Clock in ports
   clk_in1 => clk_125
 );

reset <= '0';

divider: process(clk8m) is
variable cur : u24;
begin
    if (rising_edge(clk8m)) then
        cur := count;
        cur := cur + 1;
        if (cur >= count8M) then
            cur := x"000000";
            slow_clock <= not slow_clock;
        end if;
        count <= cur;
    end if;
end process divider; 

led(0) <= slow_clock;

end clock_impl;

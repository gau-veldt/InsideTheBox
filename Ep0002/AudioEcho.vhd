----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/14/2017 12:34:40 AM
-- Design Name: 
-- Module Name: AudioEcho - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity AudioEcho is
    port (
        led         :   out std_logic_vector(3 downto 0);
        sw          :   in std_logic_vector(3 downto 0);
        clk_125     :   in std_logic
    );
end AudioEcho;

architecture Driver of AudioEcho is
subtype tCount is unsigned(31 downto 0);
constant cZero : tCount := to_unsigned(0,tCount'length);
constant cMod : tCount := to_unsigned(4,tCount'length);
constant cMax : tCount := to_unsigned(125000000,tCount'length);
signal led_st : std_logic_vector(3 downto 0) := "0001";
signal my_clk : std_logic := '0';
signal dCount : tCount := cZero;
signal pingpong : std_logic := '0';
begin

    divider: process(clk_125) is
    variable dCur : tCount := dCount;
    begin
        if (rising_edge(clk_125)) then
            dCur:=dCur+cMod;
            if (dCur >= cMax) then
                dCur := dCur - cMax;
                my_clk <= not my_clk;
            end if;
        end if;
    end process divider;

    lcount: process(my_clk) is
    variable lCur : std_logic_vector(3 downto 0) := led_st;
    begin
        if (rising_edge(my_clk)) then
            case sw is
                when "0001" =>
                    -- L to R
                    case lCur is
                        when "0001" =>
                            led_st <= "1000";
                        when "0010" =>
                            led_st <= "0001";
                        when "0100" =>
                            led_st <= "0010";
                        when "1000" =>
                            led_st <= "0100";
                        when others =>
                            led_st <= "0001";
                    end case;
                when "0010" =>
                    -- count binary
                    case lcur is
                        when "0000" =>
                            led_st <= "0001";
                        when "0001" =>
                            led_st <= "0010";
                        when "0010" =>
                            led_st <= "0011";
                        when "0011" =>
                            led_st <= "0100";
                        when "0100" =>
                            led_st <= "0101";
                        when "0101" =>
                            led_st <= "0110";
                        when "0110" =>
                            led_st <= "0111";
                        when "0111" =>
                            led_st <= "1000";
                        when "1000" =>
                            led_st <= "1001";
                        when "1001" =>
                            led_st <= "1010";
                        when "1010" =>
                            led_st <= "1011";
                        when "1011" =>
                            led_st <= "1100";
                        when "1100" =>
                            led_st <= "1101";
                        when "1101" =>
                            led_st <= "1110";
                        when "1110" =>
                            led_st <= "1111";
                        when "1111" =>
                            led_st <= "0000";
                        when others =>
                            led_st <= "0000";
                    end case;
                when "0100" =>
                    -- count gray
                    case lcur is
                        when "0000" =>
                            led_st <= "0001";
                        when "0001" =>
                            led_st <= "0011";
                        when "0011" =>
                            led_st <= "0010";
                        when "0010" =>
                            led_st <= "0110";
                        when "0110" =>
                            led_st <= "0111";
                        when "0111" =>
                            led_st <= "0101";
                        when "0101" =>
                            led_st <= "0100";
                        when "0100" =>
                            led_st <= "1100";
                        when "1100" =>
                            led_st <= "1101";
                        when "1101" =>
                            led_st <= "1111";
                        when "1111" =>
                            led_st <= "1110";
                        when "1110" =>
                            led_st <= "1010";
                        when "1010" =>
                            led_st <= "1011";
                        when "1011" =>
                            led_st <= "1001";
                        when "1001" =>
                            led_st <= "1000";
                        when "1000" =>
                            led_st <= "0000";
                        when others =>
                            led_st <= "0000";
                    end case;
                when "1000" =>
                    -- ping-pong
                    case pingpong is
                        when '0' =>
                            case lCur is
                                when "0001" =>
                                    led_st <= "0010";
                                when "0010" =>
                                    led_st <= "0100";
                                when "0100" =>
                                    led_st <= "1000";
                                when "1000" =>
                                    led_st <= "0100";
                                    pingpong <= '1';
                                when others =>
                                    led_st <= "0001";
                            end case;
                        when '1' =>
                                case lCur is
                                    when "1000" =>
                                        led_st <= "0100";
                                    when "0100" =>
                                        led_st <= "0010";
                                    when "0010" =>
                                        led_st <= "0001";
                                    when "0001" =>
                                        led_st <= "0010";
                                        pingpong <= '0';
                                    when others =>
                                        led_st <= "1000";
                                end case;
                    end case;
            when others =>
                    -- R to L
                    case lCur is
                        when "0001" =>
                            led_st <= "0010";
                        when "0010" =>
                            led_st <= "0100";
                        when "0100" =>
                            led_st <= "1000";
                        when "1000" =>
                            led_st <= "0001";
                        when others =>
                            led_st <= "0001";
                    end case;
            end case;
        end if;
    end process lcount;

    led <= led_st;

end Driver;
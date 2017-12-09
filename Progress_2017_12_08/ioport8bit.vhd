----------------------------------------------------------------------------------
--
--  8-bit I/O port
--  Contains kludge for 6510/C64 I/O port behavior
--  (in a real C64 reset sets all DDR bits to input and pullup
--  resistors on the mainboard generate 1's)
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ioport8bit is
port (
    ce      : in    std_logic;
    clk     : in    std_logic;
    res0    : in    std_logic;
    r1w0    : in    std_logic;
    a       : in    std_logic;
    din     : in    std_logic_vector(7 downto 0);
    dout    : out   std_logic_vector(7 downto 0);
    ioi     : in    std_logic_vector(7 downto 0);
    ioo     : out   std_logic_vector(7 downto 0)
);
end ioport8bit;

architecture iop8_impl of ioport8bit is

signal data     :   std_logic_vector(7 downto 0);

signal ddr      :   std_logic_vector(7 downto 0); -- 0=in, 1=out
constant d_i    :   std_logic := '0';
constant d_o    :   std_logic := '1';

signal r0w1 :   std_logic;

begin

    r0w1 <= not r1w0;

read: process (a,data,ddr,ioi) is
variable pos : unsigned(2 downto 0);
begin
    pos := "000";
    case a is
        
        when '1' =>         -- ioport
            for pos in 7 downto 0 loop
                if (ddr(pos) = d_o) then
                    dout(pos) <= data(pos);
                else
                    dout(pos) <= ioi(pos);
                end if;
            end loop;
        when others =>      -- ddr
            dout <= ddr;
    end case;
end process read;

write: process(clk,res0,ce,r0w1,a,din) is
begin
    if (res0 = '1') then
        -- kludge: Setting the first three bits to one
        ddr     <= "00101111";
        data    <= "00000111";
    elsif (rising_edge(clk)) then
        if (r0w1 = '1' and ce = '1') then
            case a is
                when '0' =>
                    ddr <= din;
                when others =>
                    data <= din;
            end case;
        end if;
    end if;
end process write;

iolines: process(data,ddr) is
variable pos : unsigned(2 downto 0);
begin
    for pos in 7 downto 0 loop
        if (ddr(pos) = '1') then
            ioo(pos) <= data(pos);
        else
            ioo(pos) <= 'Z';
        end if;
    end loop;
end process iolines;

end iop8_impl;

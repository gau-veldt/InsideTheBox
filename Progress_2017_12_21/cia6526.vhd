----------------------------------------------------------------------------------
--
--  6526 CIA
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cia6526 is
port (
    PAo             : out   std_logic_vector(7 downto 0);
    PAi             : in    std_logic_vector(7 downto 0);
    PBo             : out   std_logic_vector(7 downto 0);
    PBi             : in    std_logic_vector(7 downto 0);
    irq0            : out   std_logic;
    
    rga             : in    std_logic_vector(3 downto 0);
    rgdi            : in    std_logic_vector(7 downto 0);    
    rgdo            : out   std_logic_vector(7 downto 0);
    r1w0            : in    std_logic;
    ce              : in    std_logic;
    clk             : in    std_logic                               -- 1 MHz    
);
end cia6526;

architecture CIA_impl of cia6526 is

subtype pair is std_logic_vector(1 downto 0);
subtype slv3 is std_logic_vector(2 downto 0);
subtype nybble is std_logic_vector(3 downto 0);
subtype slv5 is std_logic_vector(4 downto 0);
subtype byte is std_logic_vector(7 downto 0);
subtype u16 is unsigned(15 downto 0);
subtype word is std_logic_vector(15 downto 0);

-- io ports A,B
signal PA               : byte := x"00";
signal PB               : byte := x"00";
signal PArd             : byte;                     -- reg 0x0
signal PBrd             : byte;                     -- reg 0x1
-- data direction registers (1=output, 0=input)
signal ddrA             : byte := x"00";            -- reg 0x2
signal ddrB             : byte := x"00";            -- reg 0x3

signal TAcnt            : u16 := x"ffff";
signal TAltc            : u16 := x"ffff";
signal TBcnt            : u16 := x"ffff";
signal TBltc            : u16 := x"ffff";

signal TActl            : byte := x"00";
signal TBctl            : byte := x"00";

alias  TArun    : std_logic is TActl(0);
alias  TAto     : std_logic is TActl(1);
alias  TAupm    : std_logic is TActl(2);
alias  TAloop   : std_logic is TActl(3);
alias  TAsrc    : pair is TActl(6 downto 5);

alias  TBrun    : std_logic is TBctl(0);
alias  TBto     : std_logic is TBctl(1);
alias  TBupm    : std_logic is TBctl(2);
alias  TBloop   : std_logic is TBctl(3);
alias  TBsrc    : pair is TBctl(6 downto 5);


signal ir_mask  : slv5 := "00000";
signal ir_ltc   : slv5 := "00000";              -- any bits set in here trigger an IRQ
signal ir_irq   : std_logic;

constant src_ATO  : integer := 0;
constant src_BTO  : integer := 1;
constant src_TOD  : integer := 2;
constant src_SDR  : integer := 3;
constant src_FLG  : integer := 4;

-- returns 1 if input is nonzero
function notZero(arg: slv5) return std_logic is
begin
    return arg(0) or arg(1) or arg(2) or arg(3) or
           arg(4);
end function notZero;

begin

ir_irq <= notZero(ir_ltc);
irq0 <= not ir_irq;

outA: process(ddrA,PA) is
begin
    for i in 7 downto 0 loop
        if (ddrA(i)='1') then
            -- dump appropriate bit of port data register to any lines flagged output
            PAo(i) <= PA(i);
        else
            -- if the line is flagged input read output a one to simluate hardware pullup
            PAo(i) <= '1';
        end if;
    end loop;
end process outA;

outB: process(ddrB,PB) is
begin
    for i in 7 downto 0 loop
        if (ddrB(i)='1') then
            -- dump appropriate bit of port data register to any lines flagged output
            PBo(i) <= PB(i);
        else
            -- if the line is flagged input read output a one to simluate hardware pullup
            PBo(i) <= '1';
        end if;
    end loop;
end process outB;

rd_A: process(ddrA,PA,PAi) is
begin
    for i in 7 downto 0 loop
        if (ddrA(i)='1') then
            -- output lines just parrot the appropriate bit in the port data register
            PArd(i) <= PA(i);
        else
            -- input lines reflect the corresponding external input lines
            PArd(i) <= PAi(i);
        end if;
    end loop;
end process rd_A;

rd_B: process(ddrB,PB,PBi) is
begin
    for i in 7 downto 0 loop
        if (ddrB(i)='1') then
            -- output lines just parrot the appropriate bit in the port data register
            PBrd(i) <= PB(i);
        else
            -- input lines reflect the corresponding external input lines
            PBrd(i) <= PBi(i);
        end if;
    end loop;
end process rd_B;

reg_access: process(clk,rgdi,ir_irq,ir_ltc,TAltc,TBltc,TAcnt,TBcnt,TActl,TBctl,PArd,PBrd,ddrA,ddrB) is
variable Acnt : u16;
variable Bcnt : u16;
begin
    Acnt := TAcnt;
    Bcnt := TBcnt;
    if (rising_edge(clk) and ce='1' and r1w0='1') then
        -- reading
        case rga is
            when x"0"   => rgdo <= PArd;
            when x"1"   => rgdo <= PBrd;
            when x"2"   => rgdo <= ddrA;
            when x"3"   => rgdo <= ddrB;
            when x"4"   => rgdo <= byte(TAcnt( 7 downto 0));
            when x"5"   => rgdo <= byte(TAcnt(15 downto 8));
            when x"6"   => rgdo <= byte(TBcnt( 7 downto 0));
            when x"7"   => rgdo <= byte(TBcnt(15 downto 8));

            when x"D"   =>
                rgdo <= ir_irq & "00" & ir_ltc;
            when x"E"   => rgdo <= TActl;
            when x"F"   => rgdo <= TBctl;
            when others => rgdo <= x"FF";
        end case;
    end if;

    if (falling_edge(clk)) then

        if (TArun='1') then
            if (Acnt=x"0000") then
                ir_ltc(src_ATO) <= ir_mask(src_ATO);
                if (TAloop='0') then
                    Acnt := TAltc;
                else
                    TArun <= '0';
                end if;
            else
                Acnt := Acnt - 1;
            end if;
        end if;

        if (TBrun='1') then
            if (Bcnt=x"0000") then
                ir_ltc(src_BTO) <= ir_mask(src_BTO);
                if (TBloop='0') then
                    Bcnt := TBltc;
                else
                    TBrun <= '0';
                end if;
            else
                Bcnt := Bcnt - 1;
            end if;
        end if;

        if (ce='1' and r1w0='1') then
            -- state-changes due to read cycles occur on downclock
            case rga is
                when x"d"   =>
                    ir_ltc <= "00000";
                when others => null;
            end case;
        end if;

        -- writing
        if (ce='1' and r1w0='0') then
            case rga is
                when x"0"   => PA   <= rgdi;
                when x"1"   => PB   <= rgdi;
                when x"2"   => ddrA <= rgdi;
                when x"3"   => ddrB <= rgdi;
                when x"4"   => TAltc( 7 downto 0) <= unsigned(rgdi);
                when x"5"   => TAltc(15 downto 8) <= unsigned(rgdi);
                when x"6"   => TBltc( 7 downto 0) <= unsigned(rgdi);
                when x"7"   => TBltc(15 downto 8) <= unsigned(rgdi);
    
                when x"D"   =>
                    if (rgdi(7)='1') then
                        -- setting
                        for i in 4 downto 0 loop
                            if (rgdi(i)='1') then
                                ir_mask(i) <= '1';
                            end if;
                        end loop;
                    else
                        -- clearing
                        for i in 4 downto 0 loop
                            if (rgdi(i)='1') then
                                ir_mask(i) <= '0';
                            end if;
                        end loop;
                    end if;
                when x"E"   =>
                    TActl <= rgdi(7 downto 5) & '0' & rgdi(3 downto 0);
                    if (rgdi(4)='1') then
                        Acnt := TAltc;
                    end if;
                when x"F"   =>
                    TBctl <= rgdi(7 downto 5) & '0' & rgdi(3 downto 0);
                    if (rgdi(4)='1') then
                        Bcnt := TBltc;
                    end if;
                when others => null;
            end case;
        end if;
        
        TAcnt <= Acnt;
        TBcnt <= Bcnt;
    end if;
end process reg_access;

end CIA_impl;

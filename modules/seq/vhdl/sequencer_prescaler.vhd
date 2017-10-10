--------------------------------------------------------------------------------
--  File:       prescaler.vhd
--  Desc:       A simple 32-bit prescaler.
--
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sequencer_prescaler is
port (
    clk_i           : in  std_logic;
    reset_i         : in  std_logic;
    PERIOD          : in  std_logic_vector(31 downto 0);
    pulse_o         : out std_logic
);
end sequencer_prescaler;

architecture rtl of sequencer_prescaler is

signal clk_cnt      : unsigned(31 downto 0) := (others => '0');

begin


pulse_o <= '1' when (clk_cnt = unsigned(PERIOD)-1) else '0';

--
-- Generate QENC clk defined by the prescaler
--
qenc_clk_gen : process(clk_i)
begin
    if rising_edge(clk_i) then
        if (reset_i = '1') then
--            pulse_o <= '0';
            clk_cnt <= (others => '0');
        else
            if (clk_cnt =  unsigned(PERIOD)-1) then
--                pulse_o <= '1';
                clk_cnt <= (others => '0');
            else
--                pulse_o <= '0';
                clk_cnt <= clk_cnt + 1;
            end if;
        end if;
    end if;
end process;

end rtl;

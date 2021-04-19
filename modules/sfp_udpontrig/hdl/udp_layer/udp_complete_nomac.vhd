--==============================================================================
-- Company:
-- Engineer:
--
-- Create Date:    09:38:49 06/13/2011
-- Design Name:
-- Module Name:    UDP_Complete_nomac - Behavioral
-- Project Name:
-- Target Devices:
-- Tool versions:
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Revision 0.02 - separated RX and TX clocks
-- Revision 0.03 - Added mac_tx_tfirst
-- Additional Comments:
--
--==============================================================================


--==============================================================================
-- Libraries Declaration
--==============================================================================
library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;

library work;
  use work.axi_types.all;
  use work.ipv4_types.all;
  use work.arp_types.all;
  use work.udp_layer_component_pkg.all;


-- Instances
--   udp_tx_block   : udp_tx
--   udp_rx_block   : udp_rx
--   ip_block       : ip_complete_nomac       IP layer


--==============================================================================
-- Entiy Declaration
--==============================================================================
entity udp_complete_nomac is
  generic (
    CLOCK_FREQ              : integer := 125000000  ; -- freq of data_in_clk -- needed to timout cntr
    ARP_TIMEOUT             : integer := 60         ; -- ARP response timeout (s)
    ARP_MAX_PKT_TMO         : integer := 5          ; -- # wrong nwk pkts received before set error
    MAX_ARP_ENTRIES         : integer := 255          -- max entries in the ARP store
  );
  port (
    -- system signals (in)
    rx_clk                  : in  std_logic;
    tx_clk                  : in  std_logic;
    reset                   : in  std_logic;
    our_ip_address          : in  std_logic_vector (31 downto 0);
    our_mac_address         : in  std_logic_vector(47 downto 0);
    control                 : in udp_control_type;
    -- status signals (out)
    arp_pkt_count           : out std_logic_vector(7 downto 0);   -- count of arp pkts received
    ip_pkt_count            : out std_logic_vector(7 downto 0);   -- number of IP pkts received for us
    -- UDP TX signals (in)
    udp_tx_start            : in  std_logic;                      -- indicates req to tx UDP
    udp_txi                 : in  udp_tx_type;                    -- UDP tx cxns
    udp_tx_result           : out std_logic_vector(1 downto 0);   -- tx status (changes during transmission)
    udp_tx_data_out_ready   : out std_logic;                      -- indicates udp_tx is ready to take data
    -- UDP RX signals (out)
    udp_rx_start            : out std_logic;                      -- indicates receipt of udp header
    udp_rxo                 : out udp_rx_type;
    -- IP RX signals (out)
    ip_rx_hdr               : out ipv4_rx_header_type;
    -- MAC Receiver (in)
    mac_rx_tdata            : in  std_logic_vector(7 downto 0);   -- data byte received
    mac_rx_tvalid           : in  std_logic;                      -- indicates tdata is valid
    mac_rx_tready           : out std_logic;                      -- tells mac that we are ready to take data
    mac_rx_tlast            : in  std_logic;                      -- indicates last byte of the trame
    -- MAC Transmitter (out)
    mac_tx_tdata            : out std_logic_vector(7 downto 0);   -- data byte to tx
    mac_tx_tvalid           : out std_logic;                      -- tdata is valid
    mac_tx_tready           : in  std_logic;                      -- mac is ready to accept data
    mac_tx_tfirst           : out std_logic;                      -- indicates first byte of frame
    mac_tx_tlast            : out std_logic                       -- indicates last byte of frame
  );
end udp_complete_nomac;



--==============================================================================
-- Architcture Declaration
--==============================================================================
architecture structural of udp_complete_nomac is

  ------------------------------------------------------------------------------
  -- Component Declaration for UDP TX / UDP RX / IP layer
  ------------------------------------------------------------------------------
  -- see udp_layer_component_pkg.vhd


  ------------------------
  -- Internal Signals
  ------------------------

  -- IP TX connectivity
  signal ip_tx_int                  : ipv4_tx_type;
  signal ip_tx_start_int            : std_logic;
  signal ip_tx_result_int           : std_logic_vector(1 downto 0);
  signal ip_tx_data_out_ready_int   : std_logic;

  -- IP RX connectivity
  signal ip_rx_int                  : ipv4_rx_type;
  signal ip_rx_start_int            : std_logic := '0';


--==============================================================================
-- Beginning of Code
--==============================================================================
begin

  -- output followers
  ip_rx_hdr <= ip_rx_int.hdr;

  -- Instantiate the UDP TX block
  udp_tx_block: udp_tx
    port map (
      -- UDP Layer signals
      udp_tx_start                => udp_tx_start,
      udp_txi                     => udp_txi,
      udp_tx_result               => udp_tx_result,
      udp_tx_data_out_ready       => udp_tx_data_out_ready,
      -- system signals
      clk                         => tx_clk,
      reset                       => reset,
      -- IP layer TX signals
      ip_tx_start                 => ip_tx_start_int,
      ip_tx                       => ip_tx_int,
      ip_tx_result                => ip_tx_result_int,
      ip_tx_data_out_ready        => ip_tx_data_out_ready_int
  );

  -- Instantiate the UDP RX block
  udp_rx_block: udp_rx
    port map (
      -- UDP Layer signals
      udp_rxo                     => udp_rxo,
      udp_rx_start                => udp_rx_start,
      -- system signals
      clk                         => rx_clk,
      reset                       => reset,
      -- IP layer RX signals
      ip_rx_start                 => ip_rx_start_int,
      ip_rx                       => ip_rx_int
    );


   ------------------------------------------------------------------------------
   -- Instantiate the IP layer
   ------------------------------------------------------------------------------
    ip_block : ip_complete_nomac
    generic map (
      use_arpv2                 => TRUE,         -- use ARP with multipule entries. for signel entry, set to FALSE
      CLOCK_FREQ                => CLOCK_FREQ,
      ARP_TIMEOUT               => ARP_TIMEOUT,
      ARP_MAX_PKT_TMO           => ARP_MAX_PKT_TMO,
      MAX_ARP_ENTRIES           => MAX_ARP_ENTRIES
    )
    port map (
      -- IP interface
      ip_tx_start               => ip_tx_start_int,
      ip_tx                     => ip_tx_int,
      ip_tx_result              => ip_tx_result_int,
      ip_tx_data_out_ready      => ip_tx_data_out_ready_int,
      ip_rx_start               => ip_rx_start_int,
      ip_rx                     => ip_rx_int,
      -- System interface
      rx_clk                    => rx_clk,
      tx_clk                    => tx_clk,
      reset                     => reset,
      our_ip_address            => our_ip_address,
      our_mac_address           => our_mac_address,
      control                   => control.ip_controls,
      -- status signals
      arp_pkt_count             => arp_pkt_count,
      ip_pkt_count              => ip_pkt_count,
      -- MAC Transmitter
      mac_tx_tdata              => mac_tx_tdata,
      mac_tx_tvalid             => mac_tx_tvalid,
      mac_tx_tready             => mac_tx_tready,
      mac_tx_tfirst             => mac_tx_tfirst,
      mac_tx_tlast              => mac_tx_tlast,
      -- MAC Receiver
      mac_rx_tdata              => mac_rx_tdata,
      mac_rx_tvalid             => mac_rx_tvalid,
      mac_rx_tready             => mac_rx_tready,
      mac_rx_tlast              => mac_rx_tlast
  );


end structural;
--==============================================================================
-- End of Code
--==============================================================================



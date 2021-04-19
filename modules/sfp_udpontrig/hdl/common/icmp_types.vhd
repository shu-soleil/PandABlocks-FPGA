--==============================================================================
-- Company        : Synchrotron SOLEIL
-- Project        : PandABox FPGA
-- Design name    : sfp_udpontrig
-- Module name    : icmp_types.vhd
-- Purpose        : this package defines types for use in ICMP (ping)
-- Author         : Thierry GARREL (ELSYS-Design)
-- Synthesizable  : YES
-- Language       : VHDL-93
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Synchrotron SOLEIL - L'Orme des Merisiers Saint-Aubin
-- BP 48 91192 Gif-sur-Yvette Cedex  - https://www.synchrotron-soleil.fr
--------------------------------------------------------------------------------


--==============================================================================
-- Libraries Declaration
--==============================================================================
library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;

library work;
  use work.axi_types.all;
  use work.ipv4_types.all;


--==============================================================================
-- Package Declaration
--==============================================================================
package icmp_types is

-- ICMP data protocol unit header format ( https://www.frameip.com/entete-icmp/ )
-- RFC972 : https://tools.ietf.org/html/rfc792
--
--    0                      8                     16                                           31
--    --------------------------------------------------------------------------------------------
--    |    Type (8 bits)     |     Code (8 bits)   |              Checksum (16 bits)             |
--    --------------------------------------------------------------------------------------------
--    |           Identifiant (16 bits)            |        Numéro de Séquence (16 bits)         |
--    --------------------------------------------------------------------------------------------
--    |                         Données (optionnel et de longueur variable)                      |
--    --------------------------------------------------------------------------------------------

-- Type = Type de message (8 bits)
-- Code = Code de l’erreur (8 bits)
-- Demande d’écho               = Type 8 Code 0
-- Réponse à une demande d’écho = Type 0 Code 0
-- Checksum (16 bits) calculée sur la partie spécifique à l’ICMP (sans l’entête IP)
-- Identifiant et Numéro de Séquence ou Bourrage (32 bits)
--
-- Les données reçues dans un message d’écho doivent être réémises dans la réponse.
-- Ainsi, si le message de retour correspond à l’émission, on en déduit que l’Hote est présent.

    ---------------------------
    -- Constants
    ---------------------------

    -- ICMP protocol : https://www.frameip.com/entete-ip/#39-8211-protocole
    -- TCP = 6, UDP = 17, ICMP = 1.
    constant C_ICMP_PROTOCOL  : std_logic_vector(7 downto 0) := x"01"; -- 01


    -- ICMP types : https://rlworkman.net/howtos/iptables/chunkyhtml/a6339.html
    --
    -- Type  Code   ICMP Type
    -- ----------------------------
    --  8     0     Echo Request
    --  0     0     Reply to echo request

    constant C_ICMP_TYPE_08   : std_logic_vector(7 downto 0) := x"08";
    constant C_ICMP_TYPE_00   : std_logic_vector(7 downto 0) := x"00";

    constant C_ICMP_CODE_00   : std_logic_vector(7 downto 0) := x"00";


    ----------------------------------------------------------------------
    -- ICMP header used by all of the ICMP types
    -- ref https://rlworkman.net/howtos/iptables/chunkyhtml/x281.html
    ----------------------------------------------------------------------
    constant C_ICMP_HEADER_LENGTH : natural := 8; -- 8 bytes


    --------------------------
    -- ICMP Header Fields
    --------------------------
    type icmp_header_type is record                  -- bytes #bytes
      src_ip_addr   : std_logic_vector(31 downto 0);
      data_length   : std_logic_vector(15 downto 0);
      msg_type      : std_logic_vector( 7 downto 0); -- 1     1
      msg_code      : std_logic_vector( 7 downto 0); -- 1     2
      checksum      : std_logic_vector(15 downto 0); -- 2     4
      identifier    : std_logic_vector(15 downto 0); -- 2     6
      seq_number    : std_logic_vector(15 downto 0); -- 2     8 bytes
    end record;

    constant C_ICMP_HEADER_NULL : icmp_header_type := (
      src_ip_addr   => (others=>'0'),
      data_length   => (others=>'0'),
      msg_type      => (others=>'0'),
      msg_code      => (others=>'0'),
      checksum      => (others=>'0'),
      identifier    => (others=>'0'),
      seq_number    => (others=>'0')

    );

    -------------
    -- ICMP Rx
    -------------
    type icmp_rx_type is record
      hdr       : icmp_header_type;
      data      : axi_in_type;
    end record;


    -------------
    -- ICMP Tx
    -------------
    type icmp_tx_type is record
      hdr       : icmp_header_type;
      data      : axi_out_type;
    end record;



end icmp_types;
--==============================================================================
-- Package End
--==============================================================================


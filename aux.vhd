-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2016-2 trabalho semestral, autor: Roberto Hexsel, 07out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador completo de um bit, modelo estrutural
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity addBit is
  port(bitA, bitB, vem : in bit;    -- entradas A,B,vem-um
       soma, vai       : out bit);  -- saida C,vai-um
end addBit;

architecture estrutural of addBit is
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;

  component or3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component or3;

  component xor3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component xor3;

  signal a1,a2,a3: bit;
begin
  U_xor:  xor3 generic map ( t_xor3 ) port map ( bitA, bitB, vem, soma );

  U_and1: and2 generic map ( t_and2 ) port map ( bitA, bitB, a1 );
  U_and2: and2 generic map ( t_and2 ) port map ( bitA, vem,  a2 );
  U_and3: and2 generic map ( t_and2 ) port map ( vem,  bitB, a3 );
  U_or:   or3  generic map ( t_or3  ) port map ( a1, a2, a3, vai );

end estrutural;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, sem adiantamento de vai-um
-- Secao 1.6+8.1.2 de RH
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adderCadeia is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
end adderCadeia;

architecture adderCadeia of adderCadeia is
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);
  end component addBit;

  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
begin

  -- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair
  U_b0: addBit port map ( inpA(0), inpB(0), vem,  r(0), v(0) );
  U_b1: addBit port map ( inpA(1), inpB(1), v(0), r(1), v(1) );
  U_b2: addBit port map ( inpA(2), inpB(2), v(1), r(2), v(2) );
  U_b3: addBit port map ( inpA(3), inpB(3), v(2), r(3), v(3) );
  U_b4: addBit port map ( inpA(4), inpB(4), v(3), r(4), v(4) );
  U_b5: addBit port map ( inpA(5), inpB(5), v(4), r(5), v(5) );
  U_b6: addBit port map ( inpA(6), inpB(6), v(5), r(6), v(6) );
  U_b7: addBit port map ( inpA(7), inpB(7), v(6), r(7), v(7) );
  U_b8: addBit port map ( inpA(8), inpB(8), v(7), r(8), v(8) );
  U_b9: addBit port map ( inpA(9), inpB(9), v(8), r(9), v(9) );
  U_ba: addBit port map ( inpA(10),inpB(10),v(9), r(10),v(10) );
  U_bb: addBit port map ( inpA(11),inpB(10),v(10),r(11),v(11) );
  U_bc: addBit port map ( inpA(12),inpB(12),v(11),r(12),v(12) );
  U_bd: addBit port map ( inpA(13),inpB(13),v(12),r(13),v(13) );
  U_be: addBit port map ( inpA(14),inpB(14),v(13),r(14),v(14) );
  U_bf: addBit port map ( inpA(15),inpB(15),v(14),r(15),v(15) );

  vai <= v(15);
  outC <= r;

end adderCadeia;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adiantamento de vai-um de 4 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adianta4 is
  port(a,b : in reg4;           -- entradas A(i),B(i)
       vem : in bit;            -- vem-um
       c: out reg4              -- vai(i)
       );
end adianta4;

architecture adianta4 of adianta4 is
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;
  component or2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component or2;

  signal p,g : reg4;
begin

  U_a0: and2 generic map ( t_and2 ) port map ( a(0), b(0), g(0) );
  U_a1: and2 generic map ( t_and2 ) port map ( a(1), b(1), g(1) );
  U_a2: and2 generic map ( t_and2 ) port map ( a(2), b(2), g(2) );
  U_a3: and2 generic map ( t_and2 ) port map ( a(3), b(3), g(3) );

  U_o0: or2 generic map ( t_or2 ) port map ( a(0), b(0), p(0) );
  U_o1: or2 generic map ( t_or2 ) port map ( a(1), b(1), p(1) );
  U_o2: or2 generic map ( t_or2 ) port map ( a(2), b(2), p(2) );
  U_o3: or2 generic map ( t_or2 ) port map ( a(3), b(3), p(3) );

  c(0) <= g(0) or (p(0) and vem) after t_and2+t_or2;
  c(1) <= g(1) or (p(1) and g(0)) or (p(1) and p(0) and vem)
          after t_and3+t_or3;
  c(2) <= g(2) or (p(2) and g(1)) or (p(2) and p(1) and g(0)) or
          (p(2) and p(1) and p(0) and vem) after t_and4+t_or4;
  c(3) <= g(3) or (p(3) and g(2)) or (p(3) and p(2) and g(1)) or
          (p(3) and p(2) and p(1) and g(0)) or
          (p(3) and p(2) and p(1) and p(0) and vem)
          after t_and5+t_or5;

end adianta4;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, com adiantamento de vai-um, 4 a 4 bits
--  P&H,2ndEd,sec4.5, RH sec8.3.2
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adderAdianta4 is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
end adderAdianta4;

architecture adderAdianta4 of adderAdianta4 is
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);
  end component addBit;

  component adianta4 port(a,b : in reg4;
                          vem : in bit;
                          c: out reg4);
  end component adianta4;

  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
  signal c : reg16;                     -- cadeia de adiantamento de vai-um
begin

  -- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair

  U_a0_3: adianta4 port map
    (inpA(3 downto 0),inpB(3 downto 0),vem,c(3 downto 0));

  U_b0: addBit port map ( inpA(0),inpB(0),vem, r(0),v(0) );
  U_b1: addBit port map ( inpA(1),inpB(1),c(0),r(1),v(1) );
  U_b2: addBit port map ( inpA(2),inpB(2),c(1),r(2),v(2) );
  U_b3: addBit port map ( inpA(3),inpB(3),c(2),r(3),v(3) );

  U_a4_7: adianta4 port map
    (inpA(7 downto 4),inpB(7 downto 4),c(3),c(7 downto 4));

  U_b4: addBit port map ( inpA(3),inpB(3),c(3),r(4),v(4) );
  U_b5: addBit port map ( inpA(5),inpB(5),c(4),r(5),v(5) );
  U_b6: addBit port map ( inpA(6),inpB(6),c(5),r(6),v(6) );
  U_b7: addBit port map ( inpA(7),inpB(7),c(6),r(7),v(7) );

  U_a8_b: adianta4 port map
    (inpA(11 downto 8),inpB(11 downto 8),c(7),c(11 downto 8));

  U_b8: addBit port map ( inpA(8), inpB(8), c(7), r(8), v(8) );
  U_b9: addBit port map ( inpA(9), inpB(9), c(8), r(9), v(9) );
  U_ba: addBit port map ( inpA(10),inpB(10),c(9), r(10),v(10) );
  U_bb: addBit port map ( inpA(11),inpB(11),c(10),r(11),v(11) );

  U_a12_15: adianta4 port map
    (inpA(15 downto 12),inpB(15 downto 12),c(10),c(15 downto 12));

  U_bc: addBit port map ( inpA(12),inpB(12),c(11),r(12),v(12) );
  U_bd: addBit port map ( inpA(13),inpB(13),c(12),r(13),v(13) );
  U_be: addBit port map ( inpA(14),inpB(14),c(13),r(14),v(14) );
  U_bf: addBit port map ( inpA(15),inpB(15),c(14),r(15),v(15) );

  vai <= c(15);
  outC <= r;

end adderAdianta4;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adiantamento de vai-um de 16 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adianta16 is
  port(a,b : in reg16;          -- entradas A(i),B(i)
       vem : in bit;            -- vem-um
       c: out reg4              -- vai(i), de 4 em 4 bits
       );
end adianta16;

architecture adianta16 of adianta16 is
  signal p,g : reg16;
  signal pp,gg,cc : reg4;
begin

  gen: for i in 15 downto 0 generate
    g(i) <= reject t_rej inertial (a(i) and b(i)) after t_and2;
    p(i) <= reject t_rej inertial (a(i) or  b(i)) after t_or2;
  end generate gen;

  pp(0) <= p(3) and p(2) and p(1) and p(0) after t_and4;
  pp(1) <= p(7) and p(6) and p(5) and p(4) after t_and4;
  pp(2) <= p(11) and p(10) and p(9) and p(8) after t_and4;
  pp(3) <= p(15) and p(14) and p(13) and p(12) after t_and4;

  gg(0) <= g(3) or (p(3) and g(2)) or (p(3) and p(2) and g(1)) or
           (p(3) and p(2) and p(1) and g(0)) after t_or4+t_and4;

  gg(1) <= g(7) or (p(7) and g(6)) or (p(7) and p(6) and g(5)) or
           (p(7) and p(6) and p(5) and g(4)) after t_or4+t_and4;

  gg(2) <= g(11) or (p(11) and g(10)) or (p(11) and p(10) and g(9)) or
           (p(11) and p(10) and p(9) and g(8)) after t_or4+t_and4;

  gg(3) <= g(15) or (p(15) and g(14)) or (p(15) and p(14) and g(13)) or
           (p(15) and p(14) and p(13) and g(12)) after t_or4+t_and4;

  cc(0) <= gg(0) or (pp(0) and vem) after t_or2+t_and2;
  cc(1) <= gg(1) or (pp(1) and gg(0)) or (pp(1) and pp(0) and vem)
           after t_or3+t_and3;
  cc(2) <= gg(2) or (pp(2) and gg(1)) or (pp(2) and pp(1) and gg(0)) or
           (pp(2) and pp(1) and pp(0) and vem) after t_or4+t_and4;
  cc(3) <= gg(3) or (pp(3) and gg(2)) or (pp(3) and pp(2) and gg(1)) or
           (pp(3) and pp(2) and pp(1) and gg(0)) or
           (pp(3) and pp(2) and pp(1) and pp(0) and vem)
           after t_or5+t_and5;

  c <= cc;

end adianta16;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, com adiantamento de vai-um de 16 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

-- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair
entity adderAdianta16 is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;             -- '0' soma, '1' subtrai
       vai  : out bit
       );
end adderAdianta16;

architecture adderAdianta16 of adderAdianta16 is
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);
  end component addBit;

  component adianta4 port(a,b : in reg4;
                          vem : in bit;
                          c: out reg4);
  end component adianta4;

  component adianta16 port(a,b : in reg16;
                          vem : in bit;
                          c: out reg4);
  end component adianta16;

  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
  signal c : reg16;
  signal cc : reg4;                     -- cadeia de adiantamento de vai-um
begin

  U_a15_0: adianta16 port map (inpA,inpB,vem,cc);

  U_a3_0: adianta4 port map
    (inpA(3 downto 0),inpB(3 downto 0),vem,c(3 downto 0));

  U_b0: addBit port map ( inpA(0),inpB(0),vem, r(0),v(0) );
  U_b1: addBit port map ( inpA(1),inpB(1),c(0),r(1),v(1) );
  U_b2: addBit port map ( inpA(2),inpB(2),c(1),r(2),v(2) );
  U_b3: addBit port map ( inpA(3),inpB(3),c(2),r(3),v(3) );

  U_a4_7: adianta4 port map
    (inpA(7 downto 4),inpB(7 downto 4),cc(0),c(7 downto 4));

  U_b4: addBit port map ( inpA(4),inpB(4),cc(0),r(4),v(4) );
  U_b5: addBit port map ( inpA(5),inpB(5), c(4),r(5),v(5) );
  U_b6: addBit port map ( inpA(6),inpB(6), c(5),r(6),v(6) );
  U_b7: addBit port map ( inpA(7),inpB(7), c(6),r(7),v(7) );

  U_a8_11: adianta4 port map
    (inpA(11 downto 8),inpB(11 downto 8),cc(1),c(11 downto 8));

  U_b8: addBit port map ( inpA(8), inpB(8), cc(0), r(8), v(8) );
  U_b9: addBit port map ( inpA(9), inpB(9),  c(8), r(9), v(9) );
  U_ba: addBit port map ( inpA(10),inpB(10), c(9),r(10),v(10) );
  U_bb: addBit port map ( inpA(11),inpB(11),c(10),r(11),v(11) );

  U_a12_15: adianta4 port map
    (inpA(15 downto 12),inpB(15 downto 12),cc(2),c(15 downto 12));

  U_bc: addBit port map ( inpA(12),inpB(12),cc(0),r(12),v(12) );
  U_bd: addBit port map ( inpA(13),inpB(13),c(12),r(13),v(13) );
  U_be: addBit port map ( inpA(14),inpB(14),c(13),r(14),v(14) );
  U_bf: addBit port map ( inpA(15),inpB(15),c(14),r(15),v(15) );

  vai <= cc(0);
  outC <= r;

end adderAdianta16;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplica por 1: A(15..0)*B(i) => S(16..0)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity m_p_1 is
  port(A,B : in  reg16;                 -- entradas A,B
       S : in bit;                      -- bit por multiplicar
       R : out reg17);                  -- produto parcial
end m_p_1;

architecture funcional of m_p_1 is

  component adderAdianta16 is port(inpA, inpB : in bit_vector;
                          outC : out bit_vector;
                          vem  : in  bit;
                          vai  : out bit);
  end component adderAdianta16;

  signal somaAB : reg17;

begin

  U_soma: adderAdianta16
    port map(A, B , somaAB(15 downto 0), '0', somaAB(16));

  R <= somaAB when S = '1' else ('0' & B);

end funcional;
-- -------------------------------------------------------------------


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplicador combinacional
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use IEEE.numeric_std.all;
use work.p_wires.all;

entity mult16x16 is
  port(A, B : in  reg16;   -- entradas A,B
       prod : out reg32);  -- produto
end mult16x16;

-- ======================================================================
-- especificação funcional para um multiplicador de 32 bits
-- ======================================================================
architecture funcional of mult16x16 is
begin
  prod <= INT2BV32( BV2INT16(A) * BV2INT16(B) );
end funcional;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- ------------------------------------------------------------------
-- descomente as linhas com --x para acrescentar o código do multiplicador
-- ------------------------------------------------------------------

 architecture estrutural of mult16x16 is

   component m_p_1 is
    port(A,B : in  reg16;   -- reg16
         S   : in  bit;
         R   : out reg17);  -- reg17
   end component m_p_1;

   signal p01,p02,p03,p04,p05,p06,p07,p08: reg17;
   signal p09,p10,p11,p12,p13,p14,p15,p16: reg17;
  begin

    U_00: m_p_1 port map (x"0000",A(15 downto 0),B(0),p01(16 downto 0));
    U_01: m_p_1 port map (p01(16 downto 1),A(15 downto 0),B(1),p02(16 downto 0));
    U_02: m_p_1 port map (p02(16 downto 1),A(15 downto 0),B(2),p03(16 downto 0));
    U_03: m_p_1 port map (p03(16 downto 1),A(15 downto 0),B(3),p04(16 downto 0));
    U_04: m_p_1 port map (p04(16 downto 1),A(15 downto 0),B(4),p05(16 downto 0));
    U_05: m_p_1 port map (p05(16 downto 1),A(15 downto 0),B(5),p06(16 downto 0));
    U_06: m_p_1 port map (p06(16 downto 1),A(15 downto 0),B(6),p07(16 downto 0));
    U_07: m_p_1 port map (p07(16 downto 1),A(15 downto 0),B(7),p08(16 downto 0));
    U_08: m_p_1 port map (p08(16 downto 1),A(15 downto 0),B(8),p09(16 downto 0));
    U_09: m_p_1 port map (p09(16 downto 1),A(15 downto 0),B(9),p10(16 downto 0));
    U_10: m_p_1 port map (p10(16 downto 1),A(15 downto 0),B(10),p11(16 downto 0));
    U_11: m_p_1 port map (p11(16 downto 1),A(15 downto 0),B(11),p12(16 downto 0));
    U_12: m_p_1 port map (p12(16 downto 1),A(15 downto 0),B(12),p13(16 downto 0));
    U_13: m_p_1 port map (p13(16 downto 1),A(15 downto 0),B(13),p14(16 downto 0));
    U_14: m_p_1 port map (p14(16 downto 1),A(15 downto 0),B(14),p15(16 downto 0));
    U_15: m_p_1 port map (p15(16 downto 1),A(15 downto 0),B(15),p16(16 downto 0));

    prod <= p16(16 downto 1) & p16(0) & p15(0) & p14(0) & p13(0) & p12(0) & p11(0) & p10(0) & p09(0)
            & p08(0) & p07(0) & p06(0) & p05(0) & p04(0) & p03(0) & p02(0) & p01(0);

end estrutural;

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- demux-4 para vetor de 16 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity demux16 is
  port(ld   : in bit;
       sel  : in reg4;
       S    : out reg16);
end demux16;

architecture functional of demux16 is
begin
  S(0) <= ld when sel = x"0" else
    '0';
  S(1) <= ld when sel = x"1" else
    '0';
  S(2) <= ld when sel = x"2" else
    '0';
  S(3) <= ld when sel = x"3" else
    '0';
  S(4) <= ld when sel = x"4" else
    '0';
  S(5) <= ld when sel = x"5" else
    '0';
  S(6) <= ld when sel = x"6" else
    '0';
  S(7) <= ld when sel = x"7" else
    '0';
  S(8) <= ld when sel = x"8" else
    '0';
  S(9) <= ld when sel = x"9" else
    '0';
  S(10) <= ld when sel = x"a" else
    '0';
  S(11) <= ld when sel = x"b" else
    '0';
  S(12) <= ld when sel = x"c" else
    '0';
  S(13) <= ld when sel = x"d" else
    '0';
  S(14) <= ld when sel = x"e" else
    '0';
  S(15) <= ld when sel = x"f" else
    '0';
end architecture functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux-16 para vetor de 32 bits
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux16 is
  port(sel  : in reg4;
       A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P : in reg32;
       S  : out reg32);
end mux16;

architecture functional of mux16 is
begin
  with sel select
  S <= A when x"0",
    B when x"1",
    C when x"2",
    D when x"3",
    E when x"4",
    F when x"5",
    G when x"6",
    H when x"7",
    I when x"8",
    J when x"9",
    K when x"a",
    L when x"b",
    M when x"c",
    N when x"d",
    O when x"e",
    P when x"f";

end architecture functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- inversor
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity inv is
  generic (prop : time := t_inv);
  port(A : in bit;
       S : out bit);
end inv;

architecture comport of inv is
begin
    S <= (not A) after prop;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta AND de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity and2 is
  generic (prop : time := t_and2);
  port(A, B : in  bit;  -- entradas A,B
       S    : out bit); -- saida C
end and2;

architecture and2 of and2 is
begin
    S <= A and B after prop;
end and2;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta OR de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or2 is
  generic (prop : time := t_or2);
  port(A,B : in bit;
       S   : out bit);
end or2;

architecture comport of or2 is
begin
  S <= reject t_rej inertial (A or B) after prop;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta OR de 3 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or3 is
  generic (prop : time := t_or3);
  port(A, B, C : in  bit;  -- entradas A,B,C
       S       : out bit); -- saida S
end or3;

architecture or3 of or3 is
begin
    S <= A or B or C after prop;
end or3;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta XOR de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor2 is
  port(A,B : in bit;
       S   : out bit);
end xor2;

architecture comport of xor2 is
begin
  S <= reject t_rej inertial (A xor B) after t_xor2;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta XOR de 3 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor3 is
  generic (prop : time := t_xor3);
  port(A, B, C : in  bit;   -- entradas A,B,C
       S       : out bit);  -- saida S
end xor3;

architecture xor3 of xor3 is
begin
    S <= A xor B xor C after prop;
end xor3;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux2(a,b,s,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux2 is
  port(A,B : in  bit;
       S   : in  bit;
       Z   : out bit);
end mux2;

architecture estrut of mux2 is
  component inv is
    generic (prop : time);
    port(A : in bit; S : out bit);
  end component inv;
  component and2 is
    generic (prop : time);
    port(A,B : in bit; S : out bit);
  end component and2;
  component or2 is
    generic (prop : time);
    port(A,B : in bit; S : out bit);
  end component or2;
  signal negs,f0,f1 : bit;
 begin

  Ui:  inv  generic map (t_inv)  port map(s,negs);
  Ua0: and2 generic map (t_and2) port map(a,negs,f0);
  Ua1: and2 generic map (t_and2) port map(b,s,f1);
  Uor: or2  generic map (t_or2)  port map(f0,f1,z);

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- N-bit register, synchronous load active in '0', asynch reset
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use work.p_WIRES.all;

entity registerN is
  generic (NUM_BITS: integer := 16;
           INIT_VAL: bit_vector);
  port(clk, rst, ld: in  bit;
       D:            in  bit_vector(NUM_BITS-1 downto 0);
       Q:            out bit_vector(NUM_BITS-1 downto 0));
end registerN;

architecture functional of registerN is
begin
  process(clk, rst, ld)
    variable state: bit_vector(NUM_BITS-1 downto 0);
  begin
    if rst = '0' then
      state := INIT_VAL;
    elsif rising_edge(clk) then
      if ld = '0' then
        state := D;
      end if;
    end if;
    Q <= state;
  end process;

end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- contador de 32 bits, reset=0 assincrono, load=1, enable=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity count32up is
  port(rel, rst, ld, en: in  bit;
        D:               in  reg32;
        Q:               out reg32);
end count32up;

architecture funcional of count32up is
  signal count: reg32;
begin

  process(rel, rst, ld)
    variable num : integer;
  begin
    if rst = '0' then
      count <= x"00000000";
    elsif ld = '1' then
      count <= D;
    elsif en = '1' and rising_edge(rel) then
      num := BV2INT(count) + 1;
      count <= INT2BV32(num);
    end if;
  end process;

  Q <= count after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- contador de 32 bits, reset=0 assincrono, load=1, enable=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity count32dwn is
  port(rel, rst, ld, en: in  bit;
        D:               in  reg32;
        Q:               out reg32);
end count32dwn;

architecture funcional of count32dwn is
  signal count: reg32;
begin

  process(rel, rst, ld)
    variable num : integer;
  begin
    if rst = '0' then
      count <= x"00000000";
    elsif ld = '1' then
      count <= D;
    elsif en = '1' and rising_edge(rel) then
      num := BV2INT(count) - 1;
      count <= INT2BV32(num);
    end if;
  end process;

  Q <= count after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 32 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador32 is
  port(rel, rst, ld: in  bit;
        D:           in  reg32;
        Q:           out reg32);
end registrador32;

architecture funcional of registrador32 is
  signal value: reg32;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <= x"00000000";
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 20 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador20 is
  port(rel, rst, ld: in  bit;
        D:           in  reg20;
        Q:           out reg20);
end registrador20;

architecture funcional of registrador20 is
  signal value: reg20;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <= (others => '0');
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- flip-flop tipo D com set,reset=0 assincronos
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;
entity FFD is
  port(rel, rst, set : in bit;
        D : in  bit;
        Q : out bit);
end FFD;

architecture funcional of FFD is
  signal estado : bit := '0';
begin

  process(rel, rst, set)
  begin
    if rst = '0' then
      estado <= '0';
    elsif set = '0' then
      estado <= '1';
    elsif rising_edge(rel) then
      estado <= D;
    end if;
  end process;

  Q <= estado after t_FFD;

end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- flip-flop tipo D com set,reset=0 assincronos, saidas Q e /Q
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;
entity FFDQQ is
  port(rel, rst, set : in bit;
        D    : in  bit;
        Q, N : out bit);
end FFDQQ;

architecture funcional of FFDQQ is
  signal estado : bit := '0';
begin

  process(rel, rst, set)
  begin
    if rst = '0' then
      estado <= '0';
    elsif set = '0' then
      estado <= '1';
    elsif rising_edge(rel) then
      estado <= D;
    end if;
  end process;

  Q <= estado after t_FFD;
  N <= not estado after t_FFD;

end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



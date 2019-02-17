--  Copyright (C) 2019 Roland Coghetto.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

-- STATUT: expérimental

pragma License (Gpl);

--with Ada.Characters.Latin_1;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
-- setfont Uni1-VGA14
-- setfont Uni1-VGA16
with Ada.Containers.Indefinite_Ordered_Maps;

package Magyar is
      type T_Lettres is
     (L_Trait,-- pas une lettre mais...
      L_A,
      L_AL,
      L_B,
      L_C,
      L_CS,
      L_D,
      L_DZ,
      L_DZS,
      L_E,
      L_EL,
      L_F,
      L_G,
      L_GY,
      L_H,
      L_I,
      L_IL,
      L_J,
      L_K,
      L_L,
      L_Ly,
      L_M,
      L_N,
      L_Ny,
      L_O,
      L_Ol,
      L_Eu,
      L_Eul,
      L_P,
      L_R,
      L_S,
      L_Sz,
      L_T,
      L_Ty,
      L_Ou,
      L_Oul,
      L_U,
      L_Ul,
      L_V,
      L_Z,
      L_Zs);

   function To_Lower_Character (L : T_Lettres) return Wide_String;

   function To_Upper_Character (L : T_Lettres) return Wide_String;

   type T_Mot is array (Natural range <>) of T_Lettres;

   function To_Wide_String (Mot : T_Mot) return Wide_String;

   type T_mots is record
      Mot      : access T_Mot;
      Variante : access T_Mot;--Access String;
   end record;
   package P_Nombres is new Ada.Containers.Indefinite_Ordered_Maps
     (Natural,
      T_Mot);
   type t_Nombres is new P_Nombres.Map with null record;
   function Init return T_Nombres;
   function To_Wide_String (Nbs : t_Nombres; N : Natural) return Wide_String;
   package Variantes is
   package P_Variantes is new Ada.Containers.Indefinite_Ordered_Maps
     (Natural,
      T_mots);
   type T_Variantes is new P_Variantes.Map with null record;




   function Init_Variantes (Nbs : t_Nombres) return T_Variantes;


   function To_Wide_String
     (Variantes : T_Variantes;
      N         : Natural;
      Variante  : Boolean := True) return Wide_String;
   end Variantes;
private
      C_En : constant T_Mot := (L_E, L_N);
   C_on : constant T_Mot := (L_O, L_N);

   C_1     : constant T_Mot := (L_E, L_GY);
   C_2     : constant T_Mot := (L_K, L_E, L_T, L_T, L_Eul);
   C_3     : constant T_Mot := (L_H, L_AL, L_R, L_O, L_M);
   C_4     : constant T_Mot := (L_N, L_EL, L_GY);
   C_5     : constant T_Mot := (L_Eu, L_T);
   C_6     : constant T_Mot := (L_H, L_A, L_T);
   C_7     : constant T_Mot := (L_H, L_EL, L_T);
   C_8     : constant T_Mot := (L_Ny, L_O, L_L, L_C);
   C_9     : constant T_Mot := (L_K, L_I, L_L, L_E, L_N, L_C);
   C_10    : constant T_Mot := (L_T, L_IL, L_Z);
   C_10bis : constant T_Mot := (L_T, L_I, L_Z);
   C_11    : constant T_Mot := C_10bis & C_En & C_1;
   C_12 : constant T_Mot := C_10bis & C_En & C_2;
   C_13 : constant T_Mot := C_10bis & C_En & C_3;
   C_14 : constant T_Mot := C_10bis & C_En & C_4;
   C_15 : constant T_Mot := C_10bis & C_En & C_5;
   C_16 : constant T_Mot := C_10bis & C_En & C_6;
   C_17 : constant T_Mot := C_10bis & C_En & C_7;
   C_18 : constant T_Mot := C_10bis & C_En & C_8;
   C_19 : constant T_Mot := C_10bis & C_En & C_9;
   C_20    : constant T_Mot := (L_H, L_Oul, L_Sz);
   C_20bis : constant T_Mot := (L_H, L_Ou, L_Sz);

   C_21 : constant T_Mot := C_20bis & C_on & C_1;
   C_22 : constant T_Mot := C_20bis & C_on & C_2;
   C_23 : constant T_Mot := C_20bis & C_on & C_3;
   C_24 : constant T_Mot := C_20bis & C_on & C_4;
   C_25 : constant T_Mot := C_20bis & C_on & C_5;
   C_26 : constant T_Mot := C_20bis & C_on & C_6;
   C_27 : constant T_Mot := C_20bis & C_on & C_7;
   C_28 : constant T_Mot := C_20bis & C_on & C_8;
   C_29 : constant T_Mot := C_20bis & C_on & C_9;

   C_30 : constant T_Mot := (L_H, L_A, L_R, L_M, L_I, L_N, L_C);
   C_31 : constant T_Mot := C_30 & C_1;
   C_32 : constant T_Mot := C_30 & C_2;
   C_33 : constant T_Mot := C_30 & C_3;
   C_34 : constant T_Mot := C_30 & C_4;
   C_35 : constant T_Mot := C_30 & C_5;
   C_36 : constant T_Mot := C_30 & C_6;
   C_37 : constant T_Mot := C_30 & C_7;
   C_38 : constant T_Mot := C_30 & C_8;
   C_39 : constant T_Mot := C_30 & C_9;
   C_40 : constant T_Mot := (L_N, L_E, L_GY, L_V, L_E, L_N);
   C_41 : constant T_Mot := C_40 & C_1;
   C_42 : constant T_Mot := C_40 & C_2;
   C_43 : constant T_Mot := C_40 & C_3;
   C_44 : constant T_Mot := C_40 & C_4;
   C_45 : constant T_Mot := C_40 & C_5;
   C_46 : constant T_Mot := C_40 & C_6;
   C_47 : constant T_Mot := C_40 & C_7;
   C_48 : constant T_Mot := C_40 & C_8;
   C_49 : constant T_Mot := C_40 & C_9;

   C_50 : constant T_Mot := (L_Eu, L_T, L_V, L_E, L_N);
   C_51 : constant T_Mot := C_50 & C_1;
   C_52 : constant T_Mot := C_50 & C_2;
   C_53 : constant T_Mot := C_50 & C_3;
   C_54 : constant T_Mot := C_50 & C_4;
   C_55 : constant T_Mot := C_50 & C_5;
   C_56 : constant T_Mot := C_50 & C_6;
   C_57 : constant T_Mot := C_50 & C_7;
   C_58 : constant T_Mot := C_50 & C_8;
   C_59 : constant T_Mot := C_50 & C_9;

   C_60 : constant T_Mot := (L_H, L_A, L_T, L_V, L_A, L_N);
   C_61 : constant T_Mot := C_60 & C_1;
   C_62 : constant T_Mot := C_60 & C_2;
   C_63 : constant T_Mot := C_60 & C_3;
   C_64 : constant T_Mot := C_60 & C_4;
   C_65 : constant T_Mot := C_60 & C_5;
   C_66 : constant T_Mot := C_60 & C_6;
   C_67 : constant T_Mot := C_60 & C_7;
   C_68 : constant T_Mot := C_60 & C_8;
   C_69 : constant T_Mot := C_60 & C_9;

   C_70 : constant T_Mot := (L_H, L_E, L_T, L_V, L_E, L_N);
   C_71 : constant T_Mot := C_70 & C_1;
   C_72 : constant T_Mot := C_70 & C_2;
   C_73 : constant T_Mot := C_70 & C_3;
   C_74 : constant T_Mot := C_70 & C_4;
   C_75 : constant T_Mot := C_70 & C_5;
   C_76 : constant T_Mot := C_70 & C_6;
   C_77 : constant T_Mot := C_70 & C_7;
   C_78 : constant T_Mot := C_70 & C_8;
   C_79 : constant T_Mot := C_70 & C_9;

   C_80 : constant T_Mot := (L_Ny, L_O, L_L, L_C, L_V, L_A, L_N);
   C_81 : constant T_Mot := C_80 & C_1;
   C_82 : constant T_Mot := C_80 & C_2;
   C_83 : constant T_Mot := C_80 & C_3;
   C_84 : constant T_Mot := C_80 & C_4;
   C_85 : constant T_Mot := C_80 & C_5;
   C_86 : constant T_Mot := C_80 & C_6;
   C_87 : constant T_Mot := C_80 & C_7;
   C_88 : constant T_Mot := C_80 & C_8;
   C_89 : constant T_Mot := C_80 & C_9;

   C_90 : constant T_Mot := (L_K, L_I, L_L, L_E, L_N, L_C, L_V, L_E, L_N);
   C_91 : constant T_Mot := C_90 & C_1;
   C_92 : constant T_Mot := C_90 & C_2;
   C_93 : constant T_Mot := C_90 & C_3;
   C_94 : constant T_Mot := C_90 & C_4;
   C_95 : constant T_Mot := C_90 & C_5;
   C_96 : constant T_Mot := C_90 & C_6;
   C_97 : constant T_Mot := C_90 & C_7;
   C_98 : constant T_Mot := C_90 & C_8;
   C_99 : constant T_Mot := C_90 & C_9;

   C_100 : constant T_Mot := (L_Sz, L_AL, L_Z);

end Magyar;

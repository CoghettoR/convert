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

package body Magyar is

   function To_Lower_Character (L : T_Lettres) return Wide_String is
   begin
      case L is
	 when L_Trait => return "-";
         when L_A =>
            return "a";
         when L_AL =>
            return "á";
         when L_B =>
            return "b";
         when L_C =>
            return "c";
         when L_CS =>
            return "cs";
         when L_D =>
            return "d";
         when L_DZ =>
            return "dz";
         when L_DZS =>
            return "dzs";
         when L_E =>
            return "e";
         when L_EL =>
            return "é";
         when L_F =>
            return "f";
         when L_G =>
            return "g";
         when L_GY =>
            return "gy";
         when L_H =>
            return "h";
         when L_I =>
            return "i";
         when L_IL =>
            return "í";
         when L_J =>
            return "j";
         when L_K =>
            return "k";
         when L_L =>
            return "l";
         when L_Ly =>
            return "ly";
         when L_M =>
            return "m";
         when L_N =>
            return "n";
         when L_Ny =>
            return "ny";
         when L_O =>
            return "o";
         when L_Ol =>
            return "ó";
         when L_Eu =>
            return "ö";
         when L_Eul =>
            return "ő";
         when L_P =>
            return "p";
         when L_R =>
            return "r";
         when L_S =>
            return "s";
         when L_Sz =>
            return "sz";
         when L_T =>
            return "t";
         when L_Ty =>
            return "ty";
         when L_Ou =>
            return "u";
         when L_Oul =>
            return "ú";
         when L_U =>
            return "ü";
         when L_Ul =>
            return "ű";
         when L_V =>
            return "v";
         when L_Z =>
            return "z";
         when L_Zs =>
            return "zs";
	    when others => return "///";
      end case;
   end To_Lower_Character;

   function To_Upper_Character (L : T_Lettres) return Wide_String is
   begin
      case L is when L_Trait => return "-";
         when L_A =>
            return "A";
         when L_AL =>
            return "Á";
         when L_B =>
            return "B";
         when L_C =>
            return "C";
         when L_CS =>
            return "CS";
         when L_D =>
            return "D";
         when L_DZ =>
            return "DZ";
         when L_DZS =>
            return "DZS";
         when L_E =>
            return "E";
         when L_EL =>
            return "É";
         when L_F =>
            return "F";
         when L_G =>
            return "G";
         when L_GY =>
            return "GY";
         when L_H =>
            return "H";
         when L_I =>
            return "I";
         when L_IL =>
            return "Í";
         when L_J =>
            return "J";
         when L_K =>
            return "K";
         when L_L =>
            return "L";
         when L_Ly =>
            return "LY";
         when L_M =>
            return "M";
         when L_N =>
            return "N";
         when L_Ny =>
            return "NY";
         when L_O =>
            return "O";
         when L_Ol =>
            return "Ó";
         when L_Eu =>
            return "Ö";
         when L_Eul =>
            return "Ő";
         when L_P =>
            return "P";
         when L_R =>
            return "R";
         when L_S =>
            return "S";
         when L_Sz =>
            return "SZ";
         when L_T =>
            return "T";
         when L_Ty =>
            return "TY";
         when L_Ou =>
            return "U";
         when L_Oul =>
            return "Ú";
         when L_U =>
            return "Ü";
         when L_Ul =>
            return "Ű";
         when L_V =>
            return "V";
         when L_Z =>
            return "Z";
         when L_Zs =>
            return "ZS";
      end case;
   end To_Upper_Character;

   function To_Wide_String (Mot : T_Mot) return Wide_String is
      Tmp : Unbounded_Wide_String;
   begin
      for I in Mot'Range loop
         Tmp := Tmp & To_Lower_Character (Mot (I));
      end loop;
      return To_Wide_String (Tmp);
   end To_Wide_String;

   function Init return t_Nombres is
      use P_Nombres;
   begin
      return Tmp : t_Nombres do
         Tmp.Insert (1, C_1);
         Tmp.Insert (2, C_2);
         Tmp.Insert (3, C_3);
         Tmp.Insert (4, C_4);
         Tmp.Insert (5, C_5);
         Tmp.Insert (6, C_6);
         Tmp.Insert (7, C_7);
         Tmp.Insert (8, C_8);
         Tmp.Insert (9, C_9);
         Tmp.Insert (10, C_10);
         Tmp.Insert (11, C_11);
         Tmp.Insert (12, C_12);
         Tmp.Insert (13, C_13);
         Tmp.Insert (14, C_14);
         Tmp.Insert (15, C_15);
         Tmp.Insert (16, C_16);
         Tmp.Insert (17, C_17);
         Tmp.Insert (18, C_18);
         Tmp.Insert (19, C_19);
         Tmp.Insert (20, C_20);
         Tmp.Insert (21, C_21);
         Tmp.Insert (22, C_22);
         Tmp.Insert (23, C_23);
         Tmp.Insert (24, C_24);
         Tmp.Insert (25, C_25);
         Tmp.Insert (26, C_26);
         Tmp.Insert (27, C_27);
         Tmp.Insert (28, C_28);
         Tmp.Insert (29, C_29);
         Tmp.Insert (30, C_30);
         Tmp.Insert (31, C_31);
         Tmp.Insert (32, C_32);
         Tmp.Insert (33, C_33);
         Tmp.Insert (34, C_34);
         Tmp.Insert (35, C_35);
         Tmp.Insert (36, C_36);
         Tmp.Insert (37, C_37);
         Tmp.Insert (38, C_38);
         Tmp.Insert (39, C_39);
         Tmp.Insert (40, C_40);
         Tmp.Insert (41, C_41);
         Tmp.Insert (42, C_42);
         Tmp.Insert (43, C_43);
         Tmp.Insert (44, C_44);
         Tmp.Insert (45, C_45);
         Tmp.Insert (46, C_46);
         Tmp.Insert (47, C_47);
         Tmp.Insert (48, C_48);
         Tmp.Insert (49, C_49);
         Tmp.Insert (50, C_50);
         Tmp.Insert (51, C_51);
         Tmp.Insert (52, C_52);
         Tmp.Insert (53, C_53);
         Tmp.Insert (54, C_54);
         Tmp.Insert (55, C_55);
         Tmp.Insert (56, C_56);
         Tmp.Insert (57, C_57);
         Tmp.Insert (58, C_58);
         Tmp.Insert (59, C_59);
         Tmp.Insert (60, C_60);
         Tmp.Insert (61, C_61);
         Tmp.Insert (62, C_62);
         Tmp.Insert (63, C_63);
         Tmp.Insert (64, C_64);
         Tmp.Insert (65, C_65);
         Tmp.Insert (66, C_66);
         Tmp.Insert (67, C_67);
         Tmp.Insert (68, C_68);
         Tmp.Insert (69, C_69);
         Tmp.Insert (70, C_70);
         Tmp.Insert (71, C_71);
         Tmp.Insert (72, C_72);
         Tmp.Insert (73, C_73);
         Tmp.Insert (74, C_74);
         Tmp.Insert (75, C_75);
         Tmp.Insert (76, C_76);
         Tmp.Insert (77, C_77);
         Tmp.Insert (78, C_78);
         Tmp.Insert (79, C_79);
         Tmp.Insert (80, C_80);
         Tmp.Insert (81, C_81);
         Tmp.Insert (82, C_82);
         Tmp.Insert (83, C_83);
         Tmp.Insert (84, C_84);
         Tmp.Insert (85, C_85);
         Tmp.Insert (86, C_86);
         Tmp.Insert (87, C_87);
         Tmp.Insert (88, C_88);
         Tmp.Insert (89, C_89);
         Tmp.Insert (90, C_90);
         Tmp.Insert (91, C_91);
         Tmp.Insert (92, C_92);
         Tmp.Insert (93, C_93);
         Tmp.Insert (94, C_94);
         Tmp.Insert (95, C_95);
         Tmp.Insert (96, C_96);
         Tmp.Insert (97, C_97);
         Tmp.Insert (98, C_98);
         Tmp.Insert (99, C_99);
         Tmp.Insert (100, C_100);
	 for I in 101..999 loop
	    declare 
	       N:Natural:=I rem 100;
	       M:Natural:=I / 100;
	    begin 
	       if M = 1 and N /= 0 then
		  declare
		     C1:T_Mot:=Element(Tmp.Find(N));
		  begin
		     Tmp.Insert(I,T_Mot'(L_Sz,L_A,L_z)&C1);
		  end;
	       elsif M > 1 then
		  if N = 0 then
		     declare
			C2:T_Mot:=Element(Tmp.Find(M));
		     begin
			Tmp.Insert(I,C2&T_Mot'(L_Sz,L_A,L_z));
		     end;		     
		  else
		     declare
			C1:T_Mot:=Element(Tmp.Find(N));
			C2:T_Mot:=Element(Tmp.Find(M));
		     begin
			Tmp.Insert(I,C2&T_Mot'(L_Sz,L_A,L_z)&C1);
		     end;
		  end if;
	       else Ada.Text_Io.Put_Line("**"&I'Img);
	       end if;
	    end;
	 end loop;
	 Tmp.Insert(1000,T_Mot'(L_E,L_Z,L_E,L_R));
	 for I in 1_001..1_999 loop
	    declare
	       C:Cursor:=Tmp.Find(I rem 1_000);
	    begin
	       Tmp.Insert(I,T_Mot'(L_E,L_Z,L_E,L_R)&Element(C));
	    end;	    
	 end loop;
	 Tmp.Insert(2000,Element(Tmp.Find(2))&Element(Tmp.Find(1000)));
	 Tmp.Insert(2001,Element(Tmp.Find(2000))&Element(Tmp.Find(1)));
	 declare
	    Ezer:T_Mot:=Element(Tmp.Find(1000));
	    begin
	 for I in 2_002..999_999 loop
	    if I rem 1000 = 0 then
	       	    Tmp.Insert(I,Element(Tmp.Find(I/1000))&ezer);
	    else
	       Tmp.Insert(I,Element(Tmp.Find(I/1000))&Ezer&L_Trait&Element(Tmp.Find(I rem 1000)));
	       end if;
	 end loop;
	    end;
	 Tmp.Insert(1E6,T_Mot'(L_M,L_I,L_L,L_L,L_I,L_Ol));
	 declare
	    Millio:T_Mot:=Element(Tmp.Find(1E6));begin
	    for I in 1E6+1..2E6 loop
	       if I rem 1E6 = 0 then
		  Tmp.Insert(I,Element(Tmp.Find(I/1E6))&Millio);
	       else
		  Tmp.Insert(I,Element(Tmp.Find(I/1E6))&Millio&L_Trait&Element(Tmp.Find(I rem 1E6)));
			     end if;
	 end loop;
	 end;
      end return;
   end Init;
      function To_Wide_String (Nbs : t_Nombres; N : Natural) return Wide_String is
      use P_Nombres;
      C : Cursor := Nbs.Find (N);
   begin
      if Has_Element (C) then
         return To_Wide_String (Element (C));
      else
         return "**";
      end if;
   end To_Wide_String;

package body Variantes is
   function Init_Variantes (Nbs : t_Nombres) return T_Variantes is
      use P_Nombres, P_Variantes;
      
   begin
      return Tmp : T_Variantes do
	for C in Nbs.Iterate loop
	   declare 
	      M:T_Mot:=Element(C);
	      M1:T_Mot(M'Range):=M;
	      I:Natural:=M'First;J:Natural:=M1'First;
	   begin
--	      Ada.Text_Io.Put(I'Img&"::"&M'Last'img);
	      loop
--		 Ada.Wide_Text_Io.Put_Line(To_Wide_String(M));
		 --	      for I in M'Range loop
--		 Ada.Text_Io.Put_Line("/"&J'Img&"/"&I'Img);
--		 M1(J):=M(I);
		 if M(I..M'Last)'Length >= 5 and then
		   --		 if 
		   M(I..I+4)=(L_K,L_E,L_T,L_T,L_eul)
		 then 
		    M1(J..J+2) := (L_K,L_El,L_T);
		    I:=I+5;J:=J+3;
		    --		    null;
		 else 
--		    Ada.Text_Io.Put("*"&M'Last'img);
		    M1(J):=M(I);
		    I:=I+1;J:=J+1;
		 end if;
		 exit when I > M'Last;
	      end loop;
	          Tmp.Insert
                  (Key
                     (C), T_mots'
                     (new T_Mot'(Element (C)),
                      new T_Mot'(M1(M1'first..J-1))));
	   end;
         end loop;
      end return;
   end Init_Variantes;


   function To_Wide_String
     (Variantes : T_Variantes;
      N         : Natural;
      Variante  : Boolean := True) return Wide_String
   is
      use P_Variantes;
      C : Cursor := Variantes.Find (N);
   begin
      if Has_Element (C) then
         case Variante is
            when True =>
               return To_Wide_String (Element (C).Variante.all);
            when False =>
               return To_Wide_String (Element (C).Mot.all);
         end case;
      else
         return "**";
      end if;
   end To_Wide_String;
end Variantes;
end Magyar;

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

with Ada.Assertions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Francais; use Francais;

procedure Test_Francais is
   type String_Ptr is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);
   
   function To_New (S : String) return String is
      S2 : String (S'Range) := S;
   begin
      for I in S'Range loop
         if S (I) = ' ' then
            if I + 5 in S'Range and then S (I + 1 .. I + 5) = "mille" then
               null;
            else
               S2 (I) := '-';
            end if;
         end if;
      end loop;
      return S2;
   end To_New;

   type T_Correction is array (1 .. 100) of access String;
   Cfrance1 : constant T_Correction :=
     (1   => new String'("un"),
      2   => new String'("deux"),
      3   => new String'("trois"),
      4   => new String'("quatre"),
      5   => new String'("cinq"),
      6   => new String'("six"),
      7   => new String'("sept"),
      8   => new String'("huit"),
      9   => new String'("neuf"),
      10  => new String'("dix"),
      11  => new String'("onze"),
      12  => new String'("douze"),
      13  => new String'("treize"),
      14  => new String'("quatorze"),
      15  => new String'("quinze"),
      16  => new String'("seize"),
      17  => new String'("dix-sept"),
      18  => new String'("dix-huit"),
      19  => new String'("dix-neuf"),
      20  => new String'("vingt"),
      21  => new String'("vingt et un"),
      22  => new String'("vingt-deux"),
      23  => new String'("vingt-trois"),
      24  => new String'("vingt-quatre"),
      25  => new String'("vingt-cinq"),
      26  => new String'("vingt-six"),
      27  => new String'("vingt-sept"),
      28  => new String'("vingt-huit"),
      29  => new String'("vingt-neuf"),
      30  => new String'("trente"),
      31  => new String'("trente et un"),
      32  => new String'("trente-deux"),
      33  => new String'("trente-trois"),
      34  => new String'("trente-quatre"),
      35  => new String'("trente-cinq"),
      36  => new String'("trente-six"),
      37  => new String'("trente-sept"),
      38  => new String'("trente-huit"),
      39  => new String'("trente-neuf"),
      40  => new String'("quarante"),
      41  => new String'("quarante et un"),
      42  => new String'("quarante-deux"),
      43  => new String'("quarante-trois"),
      44  => new String'("quarante-quatre"),
      45  => new String'("quarante-cinq"),
      46  => new String'("quarante-six"),
      47  => new String'("quarante-sept"),
      48  => new String'("quarante-huit"),
      49  => new String'("quarante-neuf"),
      50  => new String'("cinquante"),
      51  => new String'("cinquante et un"),
      52  => new String'("cinquante-deux"),
      53  => new String'("cinquante-trois"),
      54  => new String'("cinquante-quatre"),
      55  => new String'("cinquante-cinq"),
      56  => new String'("cinquante-six"),
      57  => new String'("cinquante-sept"),
      58  => new String'("cinquante-huit"),
      59  => new String'("cinquante-neuf"),
      60  => new String'("soixante"),
      61  => new String'("soixante et un"),
      62  => new String'("soixante-deux"),
      63  => new String'("soixante-trois"),
      64  => new String'("soixante-quatre"),
      65  => new String'("soixante-cinq"),
      66  => new String'("soixante-six"),
      67  => new String'("soixante-sept"),
      68  => new String'("soixante-huit"),
      69  => new String'("soixante-neuf"),
      70  => new String'("soixante-dix"),
      71  => new String'("soixante et onze"),
      72  => new String'("soixante-douze"),
      73  => new String'("soixante-treize"),
      74  => new String'("soixante-quatorze"),
      75  => new String'("soixante-quinze"),
      76  => new String'("soixante-seize"),
      77  => new String'("soixante-dix-sept"),
      78  => new String'("soixante-dix-huit"),
      79  => new String'("soixante-dix-neuf"),
      80  => new String'("quatre-vingts"),
      81  => new String'("quatre-vingt-un"),
      82  => new String'("quatre-vingt-deux"),
      83  => new String'("quatre-vingt-trois"),
      84  => new String'("quatre-vingt-quatre"),
      85  => new String'("quatre-vingt-cinq"),
      86  => new String'("quatre-vingt-six"),
      87  => new String'("quatre-vingt-sept"),
      88  => new String'("quatre-vingt-huit"),
      89  => new String'("quatre-vingt-neuf"),
      90  => new String'("quatre-vingt-dix"),
      91  => new String'("quatre-vingt-onze"),
      92  => new String'("quatre-vingt-douze"),
      93  => new String'("quatre-vingt-treize"),
      94  => new String'("quatre-vingt-quatorze"),
      95  => new String'("quatre-vingt-quinze"),
      96  => new String'("quatre-vingt-seize"),
      97  => new String'("quatre-vingt-dix-sept"),
      98  => new String'("quatre-vingt-dix-huit"),
      99  => new String'("quatre-vingt-dix-neuf"),
      100 => new String'("cent"));
   Cbelgique : constant T_Correction :=
     (1   => new String'("un"),
      2   => new String'("deux"),
      3   => new String'("trois"),
      4   => new String'("quatre"),
      5   => new String'("cinq"),
      6   => new String'("six"),
      7   => new String'("sept"),
      8   => new String'("huit"),
      9   => new String'("neuf"),
      10  => new String'("dix"),
      11  => new String'("onze"),
      12  => new String'("douze"),
      13  => new String'("treize"),
      14  => new String'("quatorze"),
      15  => new String'("quinze"),
      16  => new String'("seize"),
      17  => new String'("dix-sept"),
      18  => new String'("dix-huit"),
      19  => new String'("dix-neuf"),
      20  => new String'("vingt"),
      21  => new String'("vingt et un"),
      22  => new String'("vingt-deux"),
      23  => new String'("vingt-trois"),
      24  => new String'("vingt-quatre"),
      25  => new String'("vingt-cinq"),
      26  => new String'("vingt-six"),
      27  => new String'("vingt-sept"),
      28  => new String'("vingt-huit"),
      29  => new String'("vingt-neuf"),
      30  => new String'("trente"),
      31  => new String'("trente et un"),
      32  => new String'("trente-deux"),
      33  => new String'("trente-trois"),
      34  => new String'("trente-quatre"),
      35  => new String'("trente-cinq"),
      36  => new String'("trente-six"),
      37  => new String'("trente-sept"),
      38  => new String'("trente-huit"),
      39  => new String'("trente-neuf"),
      40  => new String'("quarante"),
      41  => new String'("quarante et un"),
      42  => new String'("quarante-deux"),
      43  => new String'("quarante-trois"),
      44  => new String'("quarante-quatre"),
      45  => new String'("quarante-cinq"),
      46  => new String'("quarante-six"),
      47  => new String'("quarante-sept"),
      48  => new String'("quarante-huit"),
      49  => new String'("quarante-neuf"),
      50  => new String'("cinquante"),
      51  => new String'("cinquante et un"),
      52  => new String'("cinquante-deux"),
      53  => new String'("cinquante-trois"),
      54  => new String'("cinquante-quatre"),
      55  => new String'("cinquante-cinq"),
      56  => new String'("cinquante-six"),
      57  => new String'("cinquante-sept"),
      58  => new String'("cinquante-huit"),
      59  => new String'("cinquante-neuf"),
      60  => new String'("soixante"),
      61  => new String'("soixante et un"),
      62  => new String'("soixante-deux"),
      63  => new String'("soixante-trois"),
      64  => new String'("soixante-quatre"),
      65  => new String'("soixante-cinq"),
      66  => new String'("soixante-six"),
      67  => new String'("soixante-sept"),
      68  => new String'("soixante-huit"),
      69  => new String'("soixante-neuf"),
      70  => new String'("septante"),
      71  => new String'("septante et un"),
      72  => new String'("septante-deux"),
      73  => new String'("septante-trois"),
      74  => new String'("septante-quatre"),
      75  => new String'("septante-cinq"),
      76  => new String'("septante-six"),
      77  => new String'("septante-sept"),
      78  => new String'("septante-huit"),
      79  => new String'("septante-neuf"),
      80  => new String'("quatre-vingts"),
      81  => new String'("quatre-vingt-un"),
      82  => new String'("quatre-vingt-deux"),
      83  => new String'("quatre-vingt-trois"),
      84  => new String'("quatre-vingt-quatre"),
      85  => new String'("quatre-vingt-cinq"),
      86  => new String'("quatre-vingt-six"),
      87  => new String'("quatre-vingt-sept"),
      88  => new String'("quatre-vingt-huit"),
      89  => new String'("quatre-vingt-neuf"),
      90  => new String'("nonante"),
      91  => new String'("nonante et un"),
      92  => new String'("nonante-deux"),
      93  => new String'("nonante-trois"),
      94  => new String'("nonante-quatre"),
      95  => new String'("nonante-cinq"),
      96  => new String'("nonante-six"),
      97  => new String'("nonante-sept"),
      98  => new String'("nonante-huit"),
      99  => new String'("nonante-neuf"),
      100 => new String'("cent"));
   procedure Test_France (Pays : Boolean) is
      cFrance : T_Correction;
      Pa      : String (1 .. 2);
      France : t_region_linguistique; -- je devrais mettre region plutot que france
   begin
      if Pays then
         cFrance := Cfrance1;
         Pa      := "FR";
         France  := Francais.france;
      else
         cFrance := Cbelgique;
         Pa      := "BE";
         France  := Belgique;
      end if;
      for I in cFrance'Range loop
         Ada.Assertions.Assert
           (to_nombre (I, France, nouvelle) = To_New (cFrance (I).all),
            "ERREUR N: " & I'Img & "ATTENDU =" & To_New (cFrance (I).all));
         Ada.Assertions.Assert
           (to_nombre (I, France, ancienne) = cFrance (I).all,
            "ERREUR A: " & I'Img);
      end loop;
      Put_Line ("PASS " & Pa & " 1..100");
      for I in 101 .. 199 loop
         Ada.Assertions.Assert
           (to_nombre (I, France, nouvelle) =
            "cent-" & To_New (cFrance (I - 100).all),
            "ERREUR N: " &
            I'Img &
            "ATTENDU =" &
            "cent-" &
            To_New (cFrance (I - 100).all));
         Ada.Assertions.Assert
           (to_nombre (I, France, ancienne) = "cent " & cFrance (I - 100).all,
            "ERREUR A: " & I'Img);
      end loop;
      Put_Line ("PASS " & Pa & " 101..199");
      --     for I in cbelgique'Range loop
      Ada.Assertions.Assert
        (to_nombre (200, France, nouvelle) = "deux-cents",
         "ERREUR N: 200" &
         to_nombre
           (200,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (200, France, ancienne) = "deux cents",
         "ERREUR A: 200");
      Put_Line ("PASS " & Pa & " 200");
      --   end loop;
      -- Ada.Assertions.assert(To_Nombre(i,belgique,Nouvelle)=To_New(Cbelgique(I).all),"ERREUR N: "&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      --Ada.Assertions.Assert(To_Nombre(i,belgique,ancienne)=Cbelgique(I).all,"ERREUR A: "&I'Img);
      Ada.Assertions.Assert
        (to_nombre (300, France, nouvelle) = "trois-cents",
         "ERREUR N: 300" &
         to_nombre
           (300,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (300, France, ancienne) = "trois cents",
         "ERREUR A: 300");
      Put_Line ("PASS " & Pa & " 300");
      Ada.Assertions.Assert
        (to_nombre (400, France, nouvelle) = "quatre-cents",
         "ERREUR N: 400" &
         to_nombre
           (400,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (400, France, ancienne) = "quatre cents",
         "ERREUR A: 400");
      Put_Line ("PASS " & Pa & " 400");
      Ada.Assertions.Assert
        (to_nombre (500, France, nouvelle) = "cinq-cents",
         "ERREUR N: 500" &
         to_nombre
           (500,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (500, France, ancienne) = "cinq cents",
         "ERREUR A: 500");
      Put_Line ("PASS " & Pa & " 500");
      Ada.Assertions.Assert
        (to_nombre (600, France, nouvelle) = "six-cents",
         "ERREUR N: 600" &
         to_nombre
           (600,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (600, France, ancienne) = "six cents",
         "ERREUR A: 600");
      Put_Line ("PASS " & Pa & " 600");
      Ada.Assertions.Assert
        (to_nombre (700, France, nouvelle) = "sept-cents",
         "ERREUR N: 700" &
         to_nombre
           (700,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (700, France, ancienne) = "sept cents",
         "ERREUR A: 700");
      Put_Line ("PASS " & Pa & " 700");
      Ada.Assertions.Assert
        (to_nombre (800, France, nouvelle) = "huit-cents",
         "ERREUR N: 800" &
         to_nombre
           (800,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (800, France, ancienne) = "huit cents",
         "ERREUR A: 800");
      Put_Line ("PASS " & Pa & " 800");
      Ada.Assertions.Assert
        (to_nombre (900, France, nouvelle) = "neuf-cents",
         "ERREUR N: 900" &
         to_nombre
           (900,
            France,
            nouvelle));--"&I'Img&"ATTENDU ="&To_New(Cbelgique(I).all));
      Ada.Assertions.Assert
        (to_nombre (900, France, ancienne) = "neuf cents",
         "ERREUR A: 900");
      Put_Line ("PASS " & Pa & " 900");

      for I in 201 .. 999 loop
         case I is
            when 201 .. 299 |
              301 .. 399    |
              401 .. 499    |
              501 .. 599    |
              601 .. 699    |
              701 .. 799    |
              801 .. 899    |
              901 .. 999    =>
               declare
                  J : constant Natural := I / 100;
               begin
                  case J is
                     when 2 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "deux-cent-" & To_New (cFrance (I - 200).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 200).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "deux cent " & cFrance (I - 200).all,
                           "ERREUR A: " & I'Img);
                     when 3 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "trois-cent-" & To_New (cFrance (I - 300).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 300).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "trois cent " & cFrance (I - 300).all,
                           "ERREUR A: " & I'Img);
                     when 4 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "quatre-cent-" & To_New (cFrance (I - 400).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 400).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "quatre cent " & cFrance (I - 400).all,
                           "ERREUR A: " & I'Img);
                     when 5 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "cinq-cent-" & To_New (cFrance (I - 500).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 500).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "cinq cent " & cFrance (I - 500).all,
                           "ERREUR A: " & I'Img);
                     when 6 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "six-cent-" & To_New (cFrance (I - 600).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 600).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "six cent " & cFrance (I - 600).all,
                           "ERREUR A: " & I'Img);
                     when 7 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "sept-cent-" & To_New (cFrance (I - 700).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 700).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "sept cent " & cFrance (I - 700).all,
                           "ERREUR A: " & I'Img);
                     when 8 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "huit-cent-" & To_New (cFrance (I - 800).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 800).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "huit cent " & cFrance (I - 800).all,
                           "ERREUR A: " & I'Img);
                     when 9 =>
                        Ada.Assertions.Assert
                          (to_nombre (I, France, nouvelle) =
                           "neuf-cent-" & To_New (cFrance (I - 900).all),
                           "ERREUR N: " &
                           I'Img &
                           "ATTENDU =" &
                           "deux-cent-" &
                           To_New (cFrance (I - 900).all));
                        Ada.Assertions.Assert
                          (to_nombre (I, France, ancienne) =
                           "neuf cent " & cFrance (I - 900).all,
                           "ERREUR A: " & I'Img);

                     when others =>
                        null;
                  end case;
               end;
            when others =>
               null;
         end case;
      end loop;
      -- end;end loop;
      Put_Line ("PASS " & Pa & " 200..999");
      Ada.Assertions.Assert
        (to_nombre (1000, France, nouvelle) = "mille",
         "ERREUR N: 1000");--"&I'Img&"ATTENDU ="&"deux-cent-"&To_New(Cfrance(I-900).all));
      Ada.Assertions.Assert
        (to_nombre (1000, France, ancienne) = "mille",
         "ERREUR A: 1000");--"&I'Img);
      Put_Line ("PASS " & Pa & " 1000");
      for I in 1001 .. 1099 loop
         Ada.Assertions.Assert
           (to_nombre (I, France, nouvelle) =
            "mille-" & To_New (cFrance (I - 1000).all),
            "ERREUR N: " &
            I'Img &
            "ATTENDU =" &
            "mille-" &
            To_New (cFrance (I - 1000).all) &
            "::" &
            to_nombre (I, France, nouvelle));
         Ada.Assertions.Assert
           (to_nombre (I, France, ancienne) =
            "mille " & cFrance (I - 1000).all,
            "ERREUR A: " & I'Img & "::" & to_nombre (I, France, ancienne));

      end loop;
      Put_Line ("PASS " & Pa & " 1001..1099");
      Ada.Assertions.Assert
        (to_nombre (1100, France, nouvelle) = "mille-cent",
         "ERREUR N: 1100");--"&I'Img&"ATTENDU ="&"deux-cent-"&To_New(Cfrance(I-900).all));
      Ada.Assertions.Assert
        (to_nombre (1100, France, ancienne) = "mille cent",
         "ERREUR A: 1100");--"&I'Img);
      Put_Line ("PASS " & Pa & " 1100");
--   Ada.Assertions.assert(To_Nombre(1100,France,Nouvelle)="mille-cent","ERREUR N: 1100");--"&I'Img&"ATTENDU ="&"deux-cent-"&To_New(Cfrance(I-900).all
      for I in 1101 .. 1999 loop
         Ada.Assertions.Assert
           (to_nombre (I, France, nouvelle) =
            "mille-" & to_nombre (I - 1000, France, nouvelle),
            "ERREUR N: " & I'Img & "::" & to_nombre (I, France, nouvelle));
         Ada.Assertions.Assert
           (to_nombre (I, France, ancienne) =
            "mille " & to_nombre (I - 1000, France, ancienne),
            "ERREUR A: " & I'Img & "::" & to_nombre (I, France, ancienne));

      end loop;
      Put_Line ("PASS " & Pa & " 1101..1999");

      for I in 2_000 .. 999_999 loop
         declare
            J     : constant Natural := I / 1000;
            K     : constant Natural := I rem 1000;
            SNOUV : constant String  := to_nombre (J, France, nouvelle);
            Lnouv : Natural          := SNOUV'Last;
            SANC  : constant String  := to_nombre (J, France, ancienne);
            Lanc  : Natural          := SANC'Last;
         begin
            -- on enlève le 's' à cent quand il est devant mille
--                     declare S:constant String:=To_Nombre(J,France,Nouvelle);begin
            if SNOUV'Length > 5
              and then SNOUV (SNOUV'Last - 4 .. SNOUV'Last) = "cents"
            then
               Lnouv := Lnouv - 1;
            end if;
            if SANC'Length > 5
              and then SANC (SANC'Last - 4 .. SANC'Last) = "cents"
            then
               Lanc := Lanc - 1;
            end if;

            if K > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, nouvelle) =
                  SNOUV (SNOUV'First .. Lnouv) &
                    "-mille-" &
                    to_nombre (K, France, nouvelle),
                  "ERREUR N: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, nouvelle));
            else
               Ada.Assertions.Assert
                 (to_nombre (I, France, nouvelle) =
                  SNOUV (SNOUV'First .. Lnouv) & "-mille",
                  "ERREUR N: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, nouvelle));
               --  end if;--end;
            end if;
            if K > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, ancienne) =
                  SANC (SANC'First .. Lanc) &
                    " mille " &
                    to_nombre (K, France, ancienne),
                  "ERREUR A: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, ancienne));
            else
--   if I > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, ancienne) =
                  SANC (SANC'First .. Lanc) & " mille",
                  "ERREUR A: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, ancienne));
            end if;
         end;
      end loop;
      Put_Line ("PASS " & Pa & " 2_000..999_999");

      for I in 1_000_000 .. 1E6 + 1 loop --999_999_999 loop
         declare
            M : String_Ptr;
            J : constant Natural := I / 1E6;
            K : constant Natural := I rem 1E6;
         begin
            --if K = 0 then
            --   Put_Line ("J=" & J'Img);
            --end if;
            if K = 0 then
               Put (J'Img & "-");
            end if;
            New_Line;

            -- regle million et milliard sont des noms invariables ?
            if J = 1 then
               M := new String'("million");
            else
               M := new String'("millions");
            end if;
            if K > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, nouvelle) =
                  to_nombre (J, France, nouvelle) &
                    " " &
                    M.all &
                    " " &
                    to_nombre (K, France, nouvelle),
                  "ERREUR N: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, nouvelle));
            else
               Ada.Assertions.Assert
                 (to_nombre (I, France, nouvelle) =
                  to_nombre (J, France, nouvelle) & " " & M.all,
                  "ERREUR N: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, nouvelle) &
                  "::" &
                  to_nombre (J, France, nouvelle) &
                  " " &
                  M.all &
                  ".");
            end if;
            if K > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, ancienne) =
                  to_nombre (J, France, ancienne) &
                    " " &
                    M.all &
                    " " &
                    to_nombre (K, France, ancienne),
                  "ERREUR A: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, ancienne));
            else
--   if I > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, ancienne) =
                  to_nombre (J, France, ancienne) & " " & M.all,
                  "ERREUR A: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, ancienne));
            end if;
            Free (M);
         end;
      end loop;

--      Put_Line ("ON COMMENCE");
      for I in 1E9 .. 1E9 + 2 loop
         declare
            M : String_Ptr;
            J : constant Natural := I / 1E9;
            K : constant Natural := I rem 1E9;
         begin
            if K = 0 then
               Put (J'Img & "-");
            end if;
            New_Line;
            -- regle million et milliard sont des noms invariables ?
            if J = 1 then
               M := new String'("milliard");
            else
               M := new String'("milliards");
            end if;
            if K > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, nouvelle) =
                  to_nombre (J, France, nouvelle) &
                    " " &
                    M.all &
                    " " &
                    to_nombre (K, France, nouvelle),
                  "ERREUR N: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, nouvelle));
            else
               Ada.Assertions.Assert
                 (to_nombre (I, France, nouvelle) =
                  to_nombre (J, France, nouvelle) & " " & M.all,
                  "ERREUR N: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, nouvelle) &
                  "::" &
                  to_nombre (J, France, nouvelle) &
                  " " &
                  M.all &
                  ".");
            end if;
            if K > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, ancienne) =
                  to_nombre (J, France, ancienne) &
                    " " &
                    M.all &
                    " " &
                    to_nombre (K, France, ancienne),
                  "ERREUR A: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, ancienne));
            else
--   if I > 0 then
               Ada.Assertions.Assert
                 (to_nombre (I, France, ancienne) =
                  to_nombre (J, France, ancienne) & " " & M.all,
                  "ERREUR A: " &
                  I'Img &
                  "::" &
                  to_nombre (I, France, ancienne));
            end if;
            Free (M);
         end;
      end loop;

      Put_Line ("PASS " & Pa & " 1_000_000..999_999_999");
   end Test_France;
begin
--      declare A:Natural;begin loop Get(A);Put_Line(To_Nombre(A,Belgique,ancienne));end loop;end;
   Ada.Assertions.Assert
     (to_nombre (81, france, nouvelle) = "quatre-vingt-un",
      "E001");
   Ada.Assertions.Assert
     (to_nombre (91, france, nouvelle) = "quatre-vingt-onze",
      "E002");
   Ada.Assertions.Assert
     (to_nombre (80, france, nouvelle) = "quatre-vingts",
      "E003");
   Ada.Assertions.Assert
     (to_nombre (83, france, nouvelle) = "quatre-vingt-trois",
      "E004");
   Ada.Assertions.Assert
     (to_nombre (400, france, ancienne) = "quatre cents",
      "E005:" & to_nombre (400, france, ancienne));
   Ada.Assertions.Assert
     (to_nombre (421, france, ancienne) = "quatre cent vingt et un",
      "E006");
   Ada.Assertions.Assert
     (to_nombre (3_000, france, ancienne) = "trois mille",
      "E007");
   Ada.Assertions.Assert
     (to_nombre (10_002, france, ancienne) = "dix mille deux",
      "E008");
   Ada.Assertions.Assert
     (to_nombre (400_000_000, france, ancienne) = "quatre cents millions",
      "E009a");
   Ada.Assertions.Assert
     (to_nombre (400_000_000, france, nouvelle) = "quatre-cents millions",
      "E009b:");--&To_Nombre(400_000_000,France,Nouvelle));
   Ada.Assertions.Assert
     (to_nombre (200_000, france, ancienne) = "deux cent mille",
      "E010a:" & to_nombre (200_000, france, ancienne));-- car suivi d'un
   Ada.Assertions.Assert
     (to_nombre (200_000, france, nouvelle) = "deux-cent-mille",
      "E010b");
   Ada.Assertions.Assert
     (to_nombre (21, france, ancienne) = "vingt et un",
      "E011");
   Ada.Assertions.Assert
     (to_nombre (21, france, nouvelle) = "vingt-et-un",
      "E012");
   Ada.Assertions.Assert
     (to_nombre (168, france, ancienne) = "cent soixante-huit",
      "E013");
   Ada.Assertions.Assert
     (to_nombre (168, france, nouvelle) = "cent-soixante-huit",
      "E014");
   Ada.Assertions.Assert
     (to_nombre (100_258, france, ancienne) =
      "cent mille deux cent cinquante-huit",
      "E015");
   Ada.Assertions.Assert
     (to_nombre (2_000_800, france, ancienne) = "deux millions huit cents",
      "E016");

   Test_France (True);
   Test_France (False);

end Test_Francais;

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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Lojban; use Lojban;
procedure Test_lojban is
   S  : constant String  := "papaki'oki'opa";
   Nb : constant Natural := 1_004_012;

   procedure Test1 is
   begin
      for I in 1_000_000 .. 1_000_001 loop
         Ada.Text_IO.Put_Line (I'Img & ":" & To_Nombre (I, Kiho => True));
      end loop;
      for I in 1 .. Natural'Last loop
         if I mod 100_000 = 0 then
            Put ("*");
         end if;
         if S = To_Nombre (I, Kiho => True) then
            Put_Line ("TROUVE:" & I'Img);
            exit;
         end if;
      end loop;
   end Test1;
begin
   Put_Line ("Avec ki'o: " & Nb'Img & ":" & To_Nombre (Nb, Kiho => True));
   Put_Line ("Sans ki'o: " & Nb'Img & ":" & To_Nombre (Nb, Kiho => False));
   New_Line;
   Put_Line ("Recherche:" & S & ":");
   Put (" --> Determination ki'o: ");
   declare
      flag   : Boolean := Ada.Strings.Fixed.Index (S, "ki'o") /= 0;
      Trouve : Boolean := False;
   begin
      if flag then
         Put_Line ("avec ki'o");
      else
         Put_Line ("sans ki'o");
      end if;
      Put (" --> Recherche: ");
      for I in 1 .. Natural'Last loop
         if I mod 100_000 = 0 then
            Put ("*");
         end if;
         if S = To_Nombre (I, Kiho => flag) then
            New_Line;
            Put (" --> Nombre trouve:" & I'Img);
            Trouve := True;
            exit;
         end if;
      end loop;
      if not Trouve then
         Put_Line
           ("ERREUR: non trouvé: soit mauvaise écriture soit supérieur à " &
            Natural'Last'Img);
      end if;
   end;
end Test_lojban;

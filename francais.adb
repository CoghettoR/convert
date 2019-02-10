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

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with digit; use digit;

package body Francais is

   function to_mot (d : T_Digit) return String is
   begin
      case To_Natural (d) is
         when 0 =>
            return "zero";
         when 1 =>
            return "un";
         when 2 =>
            return "deux";
         when 3 =>
            return "trois";
         when 4 =>
            return "quatre";
         when 5 =>
            return "cinq";
         when 6 =>
            return "six";
         when 7 =>
            return "sept";
         when 8 =>
            return "huit";
         when 9 =>
            return "neuf";
         when others =>
            raise ERREUR with "Erreur de conception programme";
      end case;
   end to_mot;

   function to_mot_dizaine (d : T_Digit) return String is
      PROGRAMME_ERREUR : exception;
   begin
      case To_Natural (d) is
         when 0 =>
            return "";
         when 1 =>
            return "dix";
         when 2 =>
            return "vingt";
         when 3 =>
            return "trente";
         when 4 =>
            return "quarante";
         when 5 =>
            return "cinquante";
         when 6 =>
            return "soixante";
         when 7 =>
            raise PROGRAMME_ERREUR;-- on doit eviter ce cas;
         --   return "***";
         when 8 =>
            return "quatre-vingt";
         when 9 =>
            return "nonante";
         when others =>
            raise ERREUR with "erreur de conception de programme";
      end case;
   end to_mot_dizaine;

   function reduce (ds : t_digits) return t_digits with
      Pre => ds'Length > 0 is
   begin
      return to_digits (to_natural (ds));
   end reduce;

   function to_nombre
     (i           : Natural;--Integer;
      variante    : t_region_linguistique;
      orthographe : t_orthographe --:= Nouvelle
      ) return String
   is

      function to_mot
        (ds          : t_digits;
         cents       : Boolean := True;
         orthographe : t_orthographe --:= Nouvelle
         ) return String with
         Pre => ds'Length > 0 is

         tmp : t_digits := reduce (ds);
         n   : Natural  := to_natural (tmp);

--      begin

--       declare
         type T_T is (Million, milliard);
         function A (N : Natural; T : T_T) return String is
            -- S:array(T_T) of String:=(Million=> "million",
            --                        Milliard=>"milliard");
            N1, N2 : Natural;
            MION   : constant String := "million";
            MIAR   : constant String := "milliard";
         --    S:access String;
         begin
            case T is
               when Million =>
                  N1 := N / 1E6;
                  N2 := N rem 1E6;
               when milliard =>
                  N1 := N / 1E9;
                  N2 := N rem 1E9;
            end case;
            if N1 = 1 then
               if N2 = 0 then
                  if T = Million then
                     return "un million";
                  else
                     return "un milliard";
                  end if;

--                        S.all;--milliard";
               else
                  if T = Million then
                     return "un million " &
                       to_nombre (N2, variante, orthographe);
                  else
                     return "un milliard " &
                       to_nombre (N2, variante, orthographe);
                  end if;
               end if;
            else
               if N2 = 0 then
                  if T = Million then
                     return to_nombre (N1, variante, orthographe) &
                       " millions";--ROLL
                  else
                     return to_nombre (N1, variante, orthographe) &
                       " milliards";--ROLL
                  end if;
               else
                  if T = Million then
                     return to_nombre (N1, variante, orthographe) &
                       " millions " &
                       to_nombre (N2, variante, orthographe);
                  else
                     return to_nombre (N1, variante, orthographe) &
                       " milliards " &
                       to_nombre (N2, variante, orthographe);
                  end if;
               end if;
            end if;
         end A;

      begin
         case n is
            when 0 =>
               return "";
            when 1 =>
               return "un";
            when 2 =>
               return "deux";
            when 3 =>
               return "trois";
            when 4 =>
               return "quatre";
            when 5 =>
               return "cinq";
            when 6 =>
               return "six";
            when 7 =>
               return "sept";
            when 8 =>
               return "huit";
            when 9 =>
               return "neuf";

            when 1E6 .. 1E9 - 1 =>
               return A (n, Million);
            when 1E9 .. Natural'Last =>
               return A (n, milliard);
            when 1E5 .. 1E6 - 1 =>
               declare
                  ds31 : t_digits (1 .. 3) :=
                    (1 => tmp (1), 2 => tmp (2), 3 => tmp (3));
                  ds32 : t_digits (1 .. 3) :=
                    (1 => tmp (4), 2 => tmp (5), 3 => tmp (6));
               begin
                  if to_natural (ds32) = 0 then
                     case orthographe is
                        when nouvelle =>
                           return to_mot
                               (ds31,
                                cents       => False,
                                orthographe => orthographe) &
                             "-mille";
                        when ancienne =>
                           return to_mot
                               (ds31,
                                cents       => False,
                                orthographe => orthographe) &
                             " mille";
                     end case;
                  else
                     case orthographe is
                        when nouvelle =>
                           return to_mot
                               (ds31,
                                cents       => False,
                                orthographe => orthographe) &
                             "-mille-" & --roll
                             to_mot
                               (ds32,
                                cents       => True,
                                orthographe => orthographe);
                        when ancienne =>
                           return to_mot
                               (ds31,
                                cents       => False,
                                orthographe => orthographe) &
                             " mille " & --roll
                             to_mot
                               (ds32,
                                cents       => True,
                                orthographe => orthographe);
                     end case;
                  end if;
               end;
            when 1E4 .. 1E5 - 1 =>
               declare
                  ds2 : t_digits (1 .. 2) := (1 => tmp (1), 2 => tmp (2));
                  ds3 : t_digits (1 .. 3) :=
                    (1 => tmp (3), 2 => tmp (4), 3 => tmp (5));
               begin
                  case orthographe is
                     when nouvelle =>
                        if to_natural (ds3) = 0 then
                           return to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe) &
                             "-mille";
                        else
                           return to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe) &
                             "-mille-" &
                             to_mot
                               (ds3,
                                cents       => True,
                                orthographe => orthographe);
                        end if;
                     when ancienne =>
                        if to_natural (ds3) = 0 then
                           return to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe) &
                             " mille";
                        else
                           return to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe) &
                             " mille " &
                             to_mot
                               (ds3,
                                cents       => True,
                                orthographe => orthographe);
                        end if;
                  end case;
               end;
            when 1E3 .. 1E4 - 1 =>
               declare
                  ds3 : t_digits (1 .. 3) :=
                    (1 => tmp (2), 2 => tmp (3), 3 => tmp (4));
               begin
                  if To_Natural (tmp (1)) = 1 then
                     if to_natural (ds3) = 0 then
                        return "mille";
                     else
                        case orthographe is
                           when nouvelle =>
--                            Ada.Text_Io.Put_Line("ok");
                              return "mille-" &
                                to_mot
                                  (reduce (ds3),
                                   cents       => True,
                                   orthographe => orthographe);
                           when ancienne =>
                              return "mille " &
                                to_mot
                                  (reduce (ds3),
                                   cents       => True,
                                   orthographe => orthographe);
                        end case;
                     end if;
                  else
                     if to_natural (ds3) = 0 then
                        case orthographe is
                           when nouvelle =>
                              return to_mot
                                  (tmp
                                     (1)) & --,Cents=>False,Orthographe=>orthographe) &
                              "-mille";
                           when ancienne =>
                              return to_mot
                                  (tmp
                                     (1)) & --,Cents=>False,Orthographe=>orthographe) &
                              " mille";
                        end case;
                     --              return "mille";
                     else
                        case orthographe is
                           when nouvelle =>
                              return to_mot
                                  (tmp
                                     (1)) & --,Cents=>False,Orthographe=>orthographe) &
                                "-mille-" &
                                to_mot
                                  (reduce (ds3),
                                   cents       => True,
                                   orthographe => orthographe);
                           when ancienne =>
                              return to_mot
                                  (tmp
                                     (1)) & -- ,Cents=>False,Orthographe=>orthographe) &
                                " mille " &
                                to_mot
                                  (reduce (ds3),
                                   cents       => True,
                                   orthographe => orthographe);
                        end case;
                     end if;
                  end if;
               end;
--          when 10=>return "dix";
            when 11 =>
               return "onze";
            when 12 =>
               return "douze";
            when 13 =>
               return "treize";
            when 14 =>
               return "quatorze";
            when 15 =>
               return "quinze";
            when 16 =>
               return "seize";
--          when 20 => return "vingt";
--          when 30 => return "trente";
--          when 40 => return "quarante";
--          when 50 => return "cinquante";
--          when 60 => return "soixante";

            when 10 | 20 | 30 | 40 | 50 | 60 =>
               return to_mot_dizaine (tmp (1));
            when 80 =>
               return to_mot_dizaine (tmp (1)) & "s";
            when 21 | 31 | 41 | 51 | 61 =>
               case orthographe is
                  when ancienne =>
                     return to_mot_dizaine (tmp (1)) &
                       " et " &
                       to_mot (tmp (2));
                  when nouvelle =>
                     return to_mot_dizaine (tmp (1)) &
                       "-et-" &
                       to_mot (tmp (2));
               end case;
            when 17 .. 19 |
              22 .. 29    |
              32 .. 39    |
              42 .. 49    |
              52 .. 59    |
              62 .. 69    |
              82 .. 89    =>
               return to_mot_dizaine (tmp (1)) & "-" & to_mot (tmp (2));
            when 70 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-dix";
                  when Belgique | RDC | suisse =>
                     return "septante";
               end case;
            when 71 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     case orthographe is
                        when ancienne =>
                           return "soixante et onze";
                        when nouvelle =>
                           return "soixante-et-onze";
                     end case;
                  when Belgique | RDC | suisse =>
                     case orthographe is
                        when ancienne =>
                           return "septante et un";
                        when nouvelle =>
                           return "septante-et-un";
                     end case;
               end case;
            when 72 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-douze";
                  when Belgique | RDC | suisse =>
                     return "septante-deux";
               end case;
            when 73 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-treize";
                  when Belgique | RDC | suisse =>
                     return "septante-trois";
               end case;
            when 74 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-quatorze";
                  when Belgique | RDC | suisse =>
                     return "septante-quatre";
               end case;
            when 75 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-quinze";
                  when Belgique | RDC | suisse =>
                     return "septante-cinq";
               end case;
            when 76 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-seize";
                  when Belgique | RDC | suisse =>
                     return "septante-six";
               end case;
            when 77 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-dix-sept";
                  when Belgique | RDC | suisse =>
                     return "septante-sept";
               end case;
            when 78 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-dix-huit";
                  when Belgique | RDC | suisse =>
                     return "septante-huit";
               end case;
            when 79 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "soixante-dix-neuf";
                  when Belgique | RDC | suisse =>
                     return "septante-neuf";
               end case;
            when 81 =>
               case variante is
--                when suisse => return "octante-un";
                  when others =>
                     return "quatre-vingt-un"; -- ancien et nouve identique
               end case;
--                   case Orthographe is when Ancienne =>
---                     return "nonante et un";
--                      when Nouvelle =>return "nonante-et-un";end case;
--             end case;

            when 90 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-dix";
                  when Belgique | RDC | suisse =>
                     return "nonante";
               end case;
            when 91 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-onze";
                  when Belgique | RDC | suisse =>
                     case orthographe is
                        when ancienne =>
                           return "nonante et un";
                        when nouvelle =>
                           return "nonante-et-un";
                     end case;
               end case;
            when 92 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-douze";
                  when Belgique | RDC | suisse =>
                     return "nonante-deux";
               end case;
            when 93 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-treize";
                  when Belgique | RDC | suisse =>
                     return "nonante-trois";
               end case;
            when 94 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-quatorze";
                  when Belgique | RDC | suisse =>
                     return "nonante-quatre";
               end case;
            when 95 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-quinze";
                  when Belgique | RDC | suisse =>
                     return "nonante-cinq";
               end case;
            when 96 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-seize";
                  when Belgique | RDC | suisse =>
                     return "nonante-six";
               end case;
            when 97 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-dix-sept";
                  when Belgique | RDC | suisse =>
                     return "nonante-sept";
               end case;
            when 98 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-dix-huit";
                  when Belgique | RDC | suisse =>
                     return "nonante-huit";
               end case;
            when 99 =>
               case variante is
                  when france                            |
                    canada                               |
                    Pays_Africains_Francophones_Sauf_RDC =>
                     return "quatre-vingt-dix-neuf";
                  when Belgique | RDC | suisse =>
                     return "nonante-neuf";
               end case;
            when 100 .. 199 =>
               declare
                  ds2 : t_digits (1 .. 2) := (1 => tmp (2), 2 => tmp (3));
               begin
                  if to_natural (ds2) = 0 then
                     return "cent";
                  else
                     case orthographe is
                        when ancienne =>
                           return "cent " &
                             to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe);
                        when nouvelle =>
                           return "cent-" &
                             to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe);
                     end case;
                  end if;
               end;
            when 200 .. 999 =>
               declare
                  ds2 : t_digits (1 .. 2) := (1 => tmp (2), 2 => tmp (3));
               begin
                  if to_natural (ds2) = 0 then
                     -- Ada.Text_Io.Put_Line("Cents="&Cents'Img);
                     if cents then
                        case orthographe is
                           when ancienne =>
                              return to_mot (tmp (1)) & " cents";
                           when nouvelle =>
                              return to_mot (tmp (1)) & "-cents";
                        end case;
                     else
                        case orthographe is
                           when ancienne =>
                              return to_mot (tmp (1)) & " cent";
                           when nouvelle =>
                              return to_mot (tmp (1)) & "-cent";
                        end case;
                     end if;
                  else
                     case orthographe is
                        when ancienne =>
                           return to_mot (tmp (1)) &
                             " cent " &
                             to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe);
                        when nouvelle =>
                           return to_mot (tmp (1)) &
                             "-cent-" &
                             to_mot
                               (ds2,
                                cents       => False,
                                orthographe => orthographe);
                     end case;
                  end if;
               end;
         end case;
      end to_mot;
   begin
      if i = 0 then
         return "zero";
      else
         return to_mot
             (to_digits (i),
              cents       => True,
              orthographe => orthographe);
      end if;
   end to_nombre;

end Francais;

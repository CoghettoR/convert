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

package Francais is
   ERREUR : exception;

   type t_orthographe is (nouvelle, ancienne);
   type t_region_linguistique is
     (france,
      canada,
      Pays_Africains_Francophones_Sauf_RDC,
      Belgique,
      RDC,
      suisse);
   function to_nombre
     (i           : Natural;
      variante    : t_region_linguistique;
      orthographe : t_orthographe) return String;

end Francais;

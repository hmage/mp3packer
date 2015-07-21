(*******************************************************************************
	This file is a part of mp3packer.

	mp3packer is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	mp3packer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with mp3packer; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*******************************************************************************)

open Mp3queue;;

let a = do_queue ~debug_in:false ~debug_queue:true ~min_bitrate:0 ~delete_beginning_junk:false ~delete_end_junk:false in

match 1 with
| 0 -> a "test/API.mp3" "out/API.mp3"
| 1 -> a "test/APS.mp3" "out/APS.mp3"
| 2 -> a "test/API 3.97b2.mp3" "out/API 3.97b2.mp3"
| 3 -> a "C:/Documents and settings/Omion/Desktop/copy of IIO - At the end CUT.mp3" "out/copy of IIO - At the end CUT.mp3"
| 4 -> a "test/ott.nonth.mp3" "out/ott.nonth.mp3"

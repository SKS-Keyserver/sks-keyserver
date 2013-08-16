function set_modifier_status(id, active)
{
	if (active)
	{
		document.getElementById(id).style.visibility = "visible";
		document.getElementById(id + ".label").style.visibility = "visible";
		document.getElementById(id).disabled = false;
		document.getElementById(id + ".label").disabled = false;
	}
	else
	{
		document.getElementById(id).style.visibility = "hidden";
		document.getElementById(id + ".label").style.visibility = "hidden";
		document.getElementById(id).disabled = true;
		document.getElementById(id + ".label").disabled = true;
	}
}




function search_options_change()
{
	var op = "";
	for (var i = 0; i < document.getElementsByName("op").length; ++i)
		if (document.getElementsByName("op")[i].checked)
		{
			op = document.getElementsByName("op")[i].value;
			break;
		}
	
	
	switch (op)
	{
	case "index":
		set_modifier_status("modifier_fingerprint", true)
		set_modifier_status("modifier_hash", true)
		set_modifier_status("modifier_options-mr", true)
		
		if (  ( document.getElementById("modifier_fingerprint").checked  ||  document.getElementById("modifier_hash").checked )  &&  document.getElementById("modifier_options-mr").checked  )
		{
			document.getElementById("modifier_options-mr").checked = false;
		}
		if (document.getElementById("modifier_options-mr").checked)
		{
			set_modifier_status("modifier_fingerprint", false)
			set_modifier_status("modifier_hash", false)
		}
		else
		{
			set_modifier_status("modifier_fingerprint", true)
			set_modifier_status("modifier_hash", true)
		}
		if (document.getElementById("modifier_fingerprint").checked  ||  document.getElementById("modifier_hash").checked)
			set_modifier_status("modifier_options-mr", false)
		else
			set_modifier_status("modifier_options-mr", true)
		
		break;
	
	case "vindex":
		set_modifier_status("modifier_fingerprint", true)
		set_modifier_status("modifier_hash", true)
		set_modifier_status("modifier_options-mr", false)
		
		break;
	
	case "get":
		set_modifier_status("modifier_fingerprint", false)
		set_modifier_status("modifier_hash", false)
		set_modifier_status("modifier_options-mr", true)
		
		break;
	
	case "hget":
		set_modifier_status("modifier_fingerprint", false)
		set_modifier_status("modifier_hash", false)
		set_modifier_status("modifier_options-mr", true)
		
		break;
	}
	
	
	
	
}
















/*
Copyright © 2010–2013, Christoph Anton Mitterer <mail@christoph.anton.mitterer.name>.
All rights reserved.


This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.
See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

This work is licensed under the Creative Commons Attribution-ShareAlike 3.0
Unported License.
To view a copy of this license, visit
http://creativecommons.org/licenses/by-sa/3.0/.

This work is licensed under the Creative Commons Attribution-ShareAlike 3.0
Germany License.
To view a copy of this license, visit
http://creativecommons.org/licenses/by-sa/3.0/de/.
*/

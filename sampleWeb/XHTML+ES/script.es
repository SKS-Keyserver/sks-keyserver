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
		document.getElementById("modifier_fingerprint").style.visibility = "visible";
		document.getElementById("modifier_hash").style.visibility = "visible";
		document.getElementById("modifier_options_mr").style.visibility = "visible";
		document.getElementById("modifier_fingerprint").disabled = false;
		document.getElementById("modifier_hash").disabled = false;
		document.getElementById("modifier_options_mr").disabled = false;
		break;
	
	case "vindex":
		document.getElementById("modifier_fingerprint").style.visibility = "visible";
		document.getElementById("modifier_hash").style.visibility = "visible";
		document.getElementById("modifier_options_mr").style.visibility = "hidden";
		document.getElementById("modifier_fingerprint").disabled = false;
		document.getElementById("modifier_hash").disabled = false;
		document.getElementById("modifier_options_mr").disabled = true;
		break;
	
	case "get":
		document.getElementById("modifier_fingerprint").style.visibility = "hidden";
		document.getElementById("modifier_hash").style.visibility = "hidden";
		document.getElementById("modifier_options_mr").style.visibility = "hidden";
		document.getElementById("modifier_fingerprint").disabled = true;
		document.getElementById("modifier_hash").disabled = true;
		document.getElementById("modifier_options_mr").disabled = true;
		break;
	
	case "hget":
		document.getElementById("modifier_fingerprint").style.visibility = "hidden";
		document.getElementById("modifier_hash").style.visibility = "hidden";
		document.getElementById("modifier_options_mr").style.visibility = "hidden";
		document.getElementById("modifier_fingerprint").disabled = true;
		document.getElementById("modifier_hash").disabled = true;
		document.getElementById("modifier_options_mr").disabled = true;
		break;
	}
}
















//Copyright © 2010, Christoph Anton Mitterer <mail@christoph.anton.mitterer.name>.
//All rights reserved.
//
//
//This work is licensed under the Creative Commons Attribution-ShareAlike 3.0
//Unported License. To view a copy of this license, visit
//http://creativecommons.org/licenses/by-sa/3.0/ or send a letter to Creative
//Commons, 171 Second Street, Suite 300, San Francisco, California, 94105, USA.
//
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.

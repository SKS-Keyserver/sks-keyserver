(***********************************************************************)
(* script.ml                                                           *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, *)
(*               2011, 2012, 2013  Yaron Minsky and Contributors       *)
(*                                                                     *)
(* This file is part of SKS.  SKS is free software; you can            *)
(* redistribute it and/or modify it under the terms of the GNU General *)
(* Public License as published by the Free Software Foundation; either *)
(* version 2 of the License, or (at your option) any later version.    *)
(*                                                                     *)
(* This program is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *)
(* General Public License for more details.                            *)
(*                                                                     *)
(* You should have received a copy of the GNU General Public License   *)
(* along with this program; if not, write to the Free Software         *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA or see <http://www.gnu.org/licenses/>.                          *)
(***********************************************************************)

#directory "bdb";;

open Common
open StdLabels
open MoreLabels
open Printf
open Bdb
open DbMessages
open Tester
open UnixLabels

module Map = PMap.Map
module Set = PSet.Set

let rec last list = match list with
    [x] -> x
  | hd::tl -> last tl
  | [] -> raise Not_found

let d1 = ADDR_UNIX "/usr/share/keyfiles/sks_wan/db_com_sock"
let r1 = ADDR_UNIX "/usr/share/keyfiles/sks_wan/recon_com_sock"
let h1 = ADDR_INET (inet_addr_any, 11371)
let h2 = ADDR_INET (inet_addr_of_string "128.84.154.32", 11371)

let get_hashes n =
  let logresp = send_msg r1 (LogQuery (n,0.)) in
  match logresp.msg with
    | LogResp loglist ->
        List.map loglist
        ~f:(function (t, Add hash) -> hash
              | (t,Delete hash) -> hash)
    | _ -> failwith "Expected LogResp"


let is_content_type line =
  try
    let colonpos = String.index line ':' in
    let prefix = String.sub ~pos:0 ~len:colonpos line in
    String.lowercase prefix = "content-type"
  with
      Not_found -> false

let get_keystrings_via_http addr hashes =
  let s = Unix.socket
            ~domain:(Unix.domain_of_sockaddr addr)
            ~kind:Unix.SOCK_STREAM
            ~protocol:0  in
  let () = Unix.connect s ~addr in
  let cin = Channel.sys_in_from_fd s
  and cout = Channel.sys_out_from_fd s in

  let sout = Channel.new_buffer_outc 0 in
  CMarshal.marshal_list ~f:CMarshal.marshal_string sout hashes;
  let msg = sout#contents in
  cout#write_string "POST /pks/hashquery HTTP/1.0\r\n";
  cout#write_string (sprintf "content-length: %d\r\n\r\n"
                       (String.length msg));
  cout#write_string msg;
  cout#flush;
  while
    not (is_content_type (input_line cin#inchan))
  do () done;
  ignore (input_line cin#inchan);
  CMarshal.unmarshal_list ~f:CMarshal.unmarshal_string cin

let get_keys addr hashes =
  List.map ~f:Key.of_string
    (get_keystrings_via_http addr (List.map ~f:KeyHash.dehexify hashes))


let test addr hashes =
  let s = Unix.socket
            ~domain:(Unix.domain_of_sockaddr addr)
            ~kind:Unix.SOCK_STREAM
            ~protocol:0  in
  let () = Unix.connect s ~addr in
  let cin = Channel.sys_in_from_fd s
  and cout = Channel.sys_out_from_fd s in

  let sout = Channel.new_buffer_outc 0 in
  CMarshal.marshal_list ~f:CMarshal.marshal_string sout hashes;
  let msg = sout#contents in
  cout#write_string "POST /pks/hashquery HTTP/1.0\r\n";
  cout#write_string (sprintf "content-length: %d\r\n\r\n"
                       (String.length msg));
  cout#write_string msg;
  cout#flush;
  cin



let hset1 = ["073AF736308A85A347C63EFFC9A99482";
             "09B4D190A6B30F86E5EFC38F0FBA2DAE";
             "0F83854955688FF6415A624B830B4DA4";
             "102D133A801CC5B52B17AEF4D566AD93";
             "109E9FFE31DD96BF8160CAB75594142D";
             "113E1742AAB522E92C2DB2491D1019D3";
             "1523B10A2C837F485CA4B9DF5321273A";
             "1ABD8F55A164E1E88B3C1F80F515F8E0";
             "1F034745CEF1BCB330274C950000F765";
             "21FCA6558FE593756B7E3F0673278CEB";
             "2223F2D7C102D79EFA575C5747A8A931";
             "254FC421BEAA8B380F833FC28D1BE2A1";
             "27B64D0F9CE17F608895F46AACD5BFF4";
             "299177B9E6B46802C14FD789F9A0C294";
             "2A521C6C82DE9FCD177B7EF9BBDCF100";
             "2ADE12449A0F2836ED6BD545EE75F2B8";
             "2AFF1B924E0397DCEE2E6ED5374219AB";
             "2BC734A43386B963F209F92B02DAA842";
             "2D6502F1D540C41D42CB4CB354C2773B";
             "2DB0521237286FEDAF6E239B7C0F6BA1";
             "2E192990A82C5055141882EE37B4B74E";
             "2F41DCA64AF8D4173CE509AFF122E1A2";
             "310F6C06F8E4A592F6B1D8F9E3E0495F";
             "31B7839521CFCFA827942A2C5C9568CA";
             "32A598E5642AFA291CE303F54F99C4D0";
             "3449CBD013A76F479F81E923685E76A8";
             "35DDA793A7E22F7EF0600BCC1F497501";
             "38D24B7A67CE0488B4243AAE8153230A";
             "3E1511FACCEF446DBA98C442D18CA0E2";
             "3E70E17FFA4E8D4D57D1FBA59364FF8B";
             "429D252EA09C27D6CE258C0EE6C98CA3";
             "4358759B254D5ACFC7D9ABAB762D0123";
             "440B43DD8FB2A3137D7D1BC67EC8F25E";
             "44F7468732F6EF97EDA177E9DFCF9848";
             "45C1E69BB78DA17F7EB31F434DD0EFA4";
             "461AA6DDE0DC9413F6F65B1868578FE1";
             "4B1EAD0ACB70AA46A6C358724051BF40";
             "4E29E8BF5F871F41295C134CAE542AE6";
             "4F4C299F3BD9584DFE59615DC088ACAF";
             "4FB4E9601A82602D4F5273E4C2C1877E";
             "50FCFA5BCEBC7AA08F3E44651E0D1877";
             "523BCB763533C5F51BB4B00EFAB97C15";
             "53F1D20CBCF7431268430BDEEB527354";
             "555761C2DF8C7877BC151CC04EF6435F";
             "56022C9F1E7493D3A11BBACD4D9DB5FA";
             "570D51EE662B2D04FEFCF79405241092";
             "5CCF177B58DC4266E4EBF8CD4780B57F";
             "5CD1C06A64DE7FF82920FA95661C1D4A";
             "5DDDC038CBABF9F492F577012FE67898";
             "61A344AE8A81C47D1EFD2139E6456F83";
             "68B1D93B7A98F3445C26A17A48CDDD9F";
             "69E80386CF505EC995D8B40C9FFEF4E5";
             "6A6B6137B0102DA560CC8A0A734E1482";
             "71735C1E66D87FC3ECAAF26B486EB313";
             "7224F58D708BFCDC69BA6AA6B5F91745";
             "72B984C808E84A2FDC5EE7E221030678";
             "73FAD4CA69D4E70D4B140956E1ADA616";
             "773382A28C6C352AC53AA27978BF11CE";
             "77E3824C7F2E466EF908209A55C6E66A";
             "7A515D5882E25F0A765D54EDFFFF5AE7";
             "7B49CF957028767419E7EAFA46B2A83B";
             "7C802BBF4618A3944DFF10AA5F861562";
             "7DAE77E172B360FC1D172B76E2669AA4";
             "7EB9074255D26AC2B72D3061297E55AD";
             "7EDE5DD999ECF3E1EA438535B0D3779D";
             "7F968E88FAFED784DCC28660BBED7478";
             "813445CB4FC4E7E4164FCF7C0CD15D36";
             "84CEB8563C8CC140C2499813327483C8";
             "8715BE02905CEF666462B8B424CA13AB";
             "8A55903F9EFC65A3C6C17E7E47ACC0EB";
             "8B63E9749AF64062AD38AF4B61FCE514";
             "8B7B36B347DED51D3296A9D47EC5516C";
             "9215976B65D0A0C85ECF5E3E946627A9";
             "92E5B775B25A058E1F548ADD168C1EBA";
             "93514EA9D63D51CC997A3FE9E1C1F499";
             "94B9EFACDBF0DA9B60688D7CB682DA06";
             "997AF31C7BD9936779057B7C55986C3D";
             "9A90D6A9205C20D7908F2B954513CDD7";
             "9B87E1E2B7A9036EABCCF5CAD305AA64";
             "9DF8C8D33B7532696FB3585CCF3C18E3";
             "9F6ADAB6E4A0AD1565BBE91772DAB754";
             "A0A2F674DEA97FDF3738EC48B8873D91";
             "A25A9B16B8453EF40BF02635CC4AC1C1";
             "A59EB873346D2546F2E7F20C3B724D90";
             "A6C976319512CD7A1BEFCFC0BB298AB4";
             "AB551FDB2B20C67204AFFD2536AF58D2";
             "AC153BA05F29E2CEDDAE3E24EDEDD92B";
             "ACE143E606EEC27A1D34DD76B99C788D";
             "AEA5DE1B43730A2D427ADF5D02FC4EBA";
             "AEC7BB1CB38C29644100F05FA71D8C14";
             "B0E1B61B479EC1AD104DF67C71E3D7D3";
             "B470D04516203E887F878496E32D5F52";
             "B6618F62F2996339DDAF98F5A551F1EC";
             "B8C4BC448D1A2A79F3D6DFE67B39A6B5";
             "B98E83A30E26E72150B90D969AC79A31";
             "BF6FB42F6907DF4DFCCBB62B3A16ACEC";
             "C0D39BC0952064F01B55BD4F7A30FC8F";
             "C139B3B14E0C6181BD6FD78EEEF0E544";
             "C3F3EBD524D1E1C379BA39F54283D4DE";
             "C479F034F4A275190D096EF34EC4C9F9";
             "C4BEA20F1231F32CC4BEEF3C09F9659D";
             "C86212FD84F29B976FA8607F3A39AA18";
             "D05A0FAFC9A407F7475DE09840C366FC";
             "D1A71BE0FDD780C8A4B6F7C852D59618";
             "D667409C8B8496C02CB8D6BA3D519E6A";
             "D7520F9ABE07F8677AF173341630A3B0";
             "D76F6D86235C359B06B0A142C4165267";
             "D9FAC58A9BD13BCE0F416C29C959691C";
             "DABF1800598B36CDD62CDEFEF269D6D9";
             "DDBDDC4B11607EAD595475700FA26BEE";
             "E291DC25E84D8FBB502159D36714B51D";
             "E351B972CE16A145D715A5139F14BEB4";
             "E4E889925A2E4670FDAE895441162462";
             "E6757E5572707ECE172B459CAE40C0D1";
             "E6E76D19638C68F93F328355D255DDC6";
             "EA49A27E0AE92A5DD202241004DEB27B";
             "EFB4EC84A1D808225028879C21868598";
             "F616BF83E695F4803EACFD522DD0FC18";
             "F65CDB83DCAA0DD5132BA175D38CB9A8";
             "F7B7186CD472AB3B2C349D1E8590A319";
             "F996B2D14287E2F48DCFE4299FA366E9";
             "FAE8908F933D8BBD76AA48E2D5D87FF5";
             "FB8822D7C59D5FDF3EE51D97B321CDA2";
             "FC4B96441E9731957BF42F9605F0CE8E";
             "FCFAB7D527718FF7DA859362FCCCCBCD"
            ]

let hset2 = [ "00B296304509111AD3FBADF5D0EB4174";
              "0380F59D0EFC08D91D92EA2DA21AA63B";
              "041F0FC43345B40966F7F719B9516BD5";
              "0AE2D4B775F1F96B69F847C41A647E7C";
              "0EC7C7B2063906583C0FB1FAF226C1F5";
              "120A8399CC40D8E77F45D0E48E24D1C3";
              "13816751FCDAD3A0C435891173F4B2B3";
              "1508AF0754BAE36164A273CE23F6F92C";
              "152C6329676F13AB7DA26920C7EE2B26";
              "17A77CD4D4C199584055F20C5BAE7E42";
              "19DFBC8796B3C601F42CBE956A21450A";
              "1AFE2D66788ACCA74623C0ABEFC76A23";
              "1B9D36DF0E1DF06C26E2CFF09EB8E8F6";
              "1DCD73D52AA4107FB10C443C7FE15D82";
              "1EFE5A704FB95F34E7681F17709CF48B";
              "21F53DA73435722B4A33AD8E592D7AD8";
              "22DEB68B528B8BFF63C67AB8E0647F5D";
              "242290617D2C2EAF3C315DBDA003FB0E";
              "25BA96D2491C28413933FF9CA65F98D1";
              "2AD37228644CBDE5B90A314BFA5F5F12";
              "2E210DBEBBA8650440087FFE78ED12E4";
              "3092D250C98AE7BD9A4E645B13047019";
              "315707A359E1335CD9A7F1B91D223304";
              "315C318F9E0D2F0808C5C2FAEDA1B9B4";
              "3469AA6260EB8016B7DE3875A540CFE3";
              "36CFC24ECE2B01AA03DB484C1606C30B";
              "3C270BDE18BA4BB6A81B65FF36DCE9DD";
              "3C753A6EF3D1147DAD75BBBAAFA78959";
              "3DD36C3D63D877766BAFF701386AE28D";
              "3FE4EE03A72DA348729D8BA178120B1A";
              "41EDD431F77E932FBE128628C387AD4C";
              "48BCFE282FC7975BBB304ACBE6491E9F";
              "49A7419829CFE787E0DAFF9972E98BBF";
              "4A4FC66FB85C5B437DB35E452AC14FA4";
              "4B763E5A0A455D7E351B4458AC37CEE3";
              "4DAA527A37447D42464337C143C59221";
              "4DDF9FC4263177FDCE3612E28619B781";
              "4F3C5D59683AA58D1E02CBD323CEEBBB";
              "4FF3987B0E59BCEA3FBB5FF691394F39";
              "51A3482B1C5E507BF977046A39B1F397";
              "537E4BC2A6459505183F9FE7D3D3C4BB";
              "59A8392563EC43A9E0BE0C4633C2383F";
              "5B91C853430935981E1222F4A3EEF3DE";
              "5D54BA1012D4C39E1642119400B26D1E";
              "5D5A87DB43E8D10507B46719F327813F";
              "5E153871907884437F90D2EA8C2AD59E";
              "5E1BFB9C0CE18CE68A11AA8BF924298C";
              "5E74618321F5DA421C185D136BD15F13";
              "65CBF754725E33FC6BBA72A12ECBA7E1";
              "6923CF692886CFAC086FFC69FFF7FD41";
              "6924AB2BC66A21ECD83AAEEF0D1B9F5D";
              "6B6F903A61AE0E06DAE159EFCCB87AB3";
              "6BA548156E1BC0BED7F1CFFB269DCEC5";
              "6D0B688753F261110D4FB1D4671157E9";
              "6EB09315AF03C56C09B0986F25919B2B";
              "6F0BDA642E5A24DDE1A1818D2DA0B802";
              "6FCF4602F565B71BF7191DE166065545";
              "70CAA0977099552575988814B67F21B2";
              "731AE161BDD59C8D1136B62AC1010042";
              "74E631C656D0AFAE7564CD33E7DE9C5F";
              "78BD52DD9FEF9CDCEC9E94F5638D1E0D";
              "792E5F84D84B2E61ED4557D914502651";
              "793ACE2504302EE88900CBCA2C0587E7";
              "7989D52BD12918F2682FDD7D094D1821";
              "7A342B342675CC29731D9F41F2E1FF88";
              "7A6208B321D39E9343F708529A195204";
              "807A560003EB5A12FA33649B2EB01EFA";
              "80E1124D15F4600AC5067F38471D4553";
              "8265D8BE769C403B894470D4218C40AF";
              "832D03E697579945398B337DCF953AD4";
              "8554C494D66FD2678D8D3E4B9392651E";
              "86A2ABC4B5B71B1AEA9AAE50F8785353";
              "892ECD9F60DB49366F6655456ED6DBE4";
              "8ABD3D82A932EC7C2460BD1F888E2D0B";
              "8D551EBFEC2992841345020BB2486D62";
              "909FB7B6F92BE1C16C98370D5541E67D";
              "91B9D87FE486EC18937D150317505790";
              "91F9C9C94C4722D37942614D4925965D";
              "970F3ED74A779CEC3970FC0D2796619C";
              "97C613AB7F49D29A98D072519F58B662";
              "9859F5F034E707F61AC61F3746506EC5";
              "99BE9C6210F952F66816CADA4659FC57";
              "9B141D0078BBF564DBE600C4D9DDB6E3";
              "A12AA1F81B55BA7E860F104F70B28CE5";
              "A30051E27A50A4A2B08C5B0297308B29";
              "A78EBF9942E2BB2A19BFF54264C1DF92";
              "A8A9C12ABABFFC0BEA23096027224813";
              "A91B09ECED33D833BB96E4A94102B666";
              "AB5528D81E3D5A57CB6267CD178F2EDB";
              "AC73E0342A0391349AA054FA4A1A8FA9";
              "AD4ADBCFFF4554F8998A6173DAAEEF85";
              "ADD59B5ABBFD060E37A432E341BE3F0C";
              "AF7B930A794E2B0D5F08EEE4D3D39C10";
              "B13F9377018068081D44A16446EFFCF2";
              "B37A6AF07852F39A863677F22EDF8E12";
              "B9FA82C1DB741A85D8E519A7CDE7ACCC";
              "C577251814B145B6B5B59D8F42D0DF26";
              "C88871174AFDB0C46873D75C614CDE10";
              "CBADA8F7FBD2DEB89129EC3BB36328CD";
              "CCA68A5581EA50E3D72E39726C7CCA8B";
              "CD46C59E67F9A2165DA7030E11C72D5C";
              "D338A6A91057EDADF8A1AD3BFA116D13";
              "D34DB4C12B869BC3590219F2C584B60E";
              "D4F9A8AC603954243E7E23843352C2C9";
              "D5DECE3E600DA5211D1C49DCA4327B17";
              "D7809C3C317CED042341D980C4579284";
              "DA13BC09EBFBB6CF83E41027FB3A14A1";
              "DBE411983E0C37A11CF0F90592A7491F";
              "DF288C042F30691D52204F8A57A8E746";
              "E06ED99CC839B8680E465A96042C185C";
              "E0DE6722AB921D1CB3E17CA59E1A6A9E";
              "E1A3585BD925180FD594C6E75CC15DE8";
              "E1BA428E3CF44E78600E4EC9DA19D14C";
              "E2BB667A2B4A35BAA683CCF1457914E0";
              "E360F4EBAB65F8906E6BE774D982B13C";
              "E4C48C99B4FF0CE7584E23C2198E4556";
              "E540ADE2CE01D310C72BD8222F1EB17E";
              "E6D7221D91D4EAE9BC1D7EBAD253C059";
              "EE0B194B94A807D85139A121D0D7582F";
              "F3E9BA4BB990F09022B4A20D567AC376";
              "F5FA58C9C0A4CB9ED534342C8DDAB355";
              "F6DF7D2DBA71E52B600032C7A22767C5";
              "F7961C3436DB73A9BDDEEC0A5205F5F1";
              "FA7DD2C088E88B0CCEBE8BD232DBD935";
              "FAD188976DB9B5C8CA0704B30A108747";
              "FC6D9BE059D84DBAFDF38D38326BB0E2"]

let (|=) map key = Map.find key map
let (|<) map key = (fun data -> Map.add ~key ~data map)

let keys1 = get_keys h1 hset2
let keys2 = get_keys h2 hset1

let kmap1 = Map.of_alist
             (List.map keys1
                ~f:(fun key -> (Fingerprint.keyid_from_key key, key)))

let kmap2 = Map.of_alist
             (List.map keys2
                ~f:(fun key -> (Fingerprint.keyid_from_key key, key)))

let unwrap x = match x with None -> failwith "unwrap failed" | Some x -> x

let subset k1 k2 = Set.subset (Set.of_list k1) (Set.of_list k2)
let equal k1 k2 = Set.equal (Set.of_list k1) (Set.of_list k2)



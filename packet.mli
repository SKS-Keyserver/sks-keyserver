type ptype =
    Reserved
  | Public_Key_Encrypted_Session_Key_Packet
  | Signature_Packet
  | Symmetric_Key_Encrypted_Session_Key_Packet
  | One_Pass_Signature_Packet
  | Secret_Key_Packet
  | Public_Key_Packet
  | Secret_Subkey_Packet
  | Compressed_Data_Packet
  | Symmetrically_Encrypted_Data_Packet
  | Marker_Packet
  | Literal_Data_Packet
  | Trust_Packet
  | User_ID_Packet
  | User_Attribute_Packet
  | Sym_Encrypted_and_Integrity_Protected_Data_Packet
  | Modification_Detection_Code_Packet
  | Public_Subkey_Packet
  | Private_or_Experimental_ptype
  | Unexpected_ptype
type packet = {
  content_tag : int;
  packet_type : ptype;
  packet_length : int;
  packet_body : string;
}
type sigsubpacket = { ssp_length : int; ssp_type : int; ssp_body : string; }
val ssp_type_to_string : int -> string
type key = packet list
val sigtype_to_string : int -> string
val content_tag_to_ptype : int -> ptype
val ptype_to_string : ptype -> string
type mpi = { mpi_bits : int; mpi_data : string; }
val pubkey_algorithm_string : int -> string
type pubkeyinfo = {
  pk_version : int;
  pk_ctime : int64;
  pk_expiration : int option;
  pk_alg : int;
  pk_keylen : int;
}
type sigtype =
    Signature_of_a_binary_document
  | Signature_of_a_canonical_text_document
  | Standalone_signature
  | Generic_certification_of_a_User_ID_and_Public_Key_packet
  | Persona_certification_of_a_User_ID_and_Public_Key_packet
  | Casual_certification_of_a_User_ID_and_Public_Key_packet
  | Positive_certification_of_a_User_ID_and_Public_Key_packet
  | Subkey_Binding_Signature
  | Signature_directly_on_a_key
  | Key_revocation_signature
  | Subkey_revocation_signature
  | Certification_revocation_signature
  | Timestamp_signature
  | Unexpected_sigtype
type v3sig = {
  v3s_sigtype : int;
  v3s_ctime : int64;
  v3s_keyid : string;
  v3s_pk_alg : int;
  v3s_hash_alg : int;
  v3s_hash_value : string;
  v3s_mpis : mpi list;
}
type v4sig = {
  v4s_sigtype : int;
  v4s_pk_alg : int;
  v4s_hashed_subpackets : sigsubpacket list;
  v4s_unhashed_subpackets : sigsubpacket list;
  v4s_hash_value : string;
  v4s_mpis : mpi list;
}
type signature = V3sig of v3sig | V4sig of v4sig
val int_to_sigtype : int -> sigtype
val content_tag_to_string : int -> string
val print_packet : packet -> unit
val write_packet_new :
  packet ->
  < write_byte : int -> 'a; write_int : int -> 'b;
    write_string : string -> 'c; .. > ->
  'c
val pk_alg_to_ident : int -> string
val write_packet_old :
  packet ->
  < write_byte : int -> 'a; write_int : int -> 'b;
    write_string : string -> 'c; .. > ->
  'c
val write_packet :
  packet ->
  < write_byte : int -> 'a; write_int : int -> 'b;
    write_string : string -> 'c; .. > ->
  'c

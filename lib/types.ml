type t_node =
  | IntNode of int
  | StrNode of string
  | ListNode of t_node list

type t_list = t_node list

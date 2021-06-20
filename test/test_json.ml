(* let test_01 =
 *   Json.print ([])
 * in
 * test_01 *)

(* let test_02 =
 *   Json.print ([Int 1])
 * in
 * test_02 *)

(* let test_03 =
 *   Json.print ([String "fdsa"])
 * in
 * test_03 *)

(* let test_04 =
 *   Json.print ([
 *         Int (-123);
 *         String "fdsa"
 *     ])
 * in
 * test_04 *)

(* let test_05 =
 *   Json.print ([
 *         List []
 *     ])
 * in
 * test_05 *)

(* let test_06 =
 *   Json.print (
 *       List [
 *           Int 1;
 *           String "a";
 *           List [
 *               Int 2;
 *               String "b"
 *             ];
 *           Int 3;
 *           String "c"
 *         ]
 *     )
 * in
 * test_06 *)

(* let test_07 =
 *   Json.print (
 *       List [
 *         String "漢字";
 *       ]
 *     )
 * in
 * test_07 *)

let () =
  let json = Utils.read_stdin_all () in
  let data : 'a list = Json.parse json in
  Json.pretty_print data

(* Licensed under the Academic Free License version 2.1, <https://spdx.org/licenses/AFL-2.1.html> *)
(* SPDX-FileCopyrightText: © 2003 Kristian Thy <thy@42.dk>                                        *)
(* SPDX-License-Identifier: AFL-2.1                                                               *)

load "Int";    (* Functions on Integers   *)
load "Random"; (* Random number functions *)
load "Array2"; (* 2D arrays; aka matrices *)

structure Des :> DES =
    struct

        exception NotBinary;

        (* start S-boxes *)
        val zero     = [0,0,0,0];
        val one      = [0,0,0,1];
        val two      = [0,0,1,0];
        val three    = [0,0,1,1];
        val four     = [0,1,0,0];
        val five     = [0,1,0,1];
        val six      = [0,1,1,0];
        val seven    = [0,1,1,1];
        val eight    = [1,0,0,0];
        val nine     = [1,0,0,1];
        val ten      = [1,0,1,0];
        val eleven   = [1,0,1,1];
        val twelve   = [1,1,0,0];
        val thirteen = [1,1,0,1];
        val fourteen = [1,1,1,0];
        val fifteen  = [1,1,1,1];

        val S0 = Array2.fromList([[fourteen,four,thirteen,one,two,fifteen,eleven,eight,three,ten,six,twelve,five,nine,zero,seven],
                                  [zero,fifteen,seven,four,fourteen,two,thirteen,one,ten,six,twelve,eleven,nine,five,three,eight],
                                  [four,one,fourteen,eight,thirteen,six,two,eleven,fifteen,twelve,nine,seven,three,ten,five,zero],
                                  [fifteen,twelve,eight,two,four,nine,one,seven,five,eleven,three,fourteen,ten,zero,six,thirteen]]);
        val S1 = Array2.fromList([[fifteen,one,eight,fourteen,six,eleven,three,four,nine,seven,two,thirteen,twelve,zero,five,ten],
                                  [three,thirteen,four,seven,fifteen,two,eight,fourteen,twelve,zero,one,ten,six,nine,eleven,five],
                                  [zero,fourteen,seven,eleven,ten,four,thirteen,one,five,eight,twelve,six,nine,three,two,fifteen],
                                  [thirteen,eight,ten,one,three,fifteen,four,two,eleven,six,seven,twelve,zero,five,fourteen,nine]]);
        val S2 = Array2.fromList([[ten,zero,nine,fourteen,six,three,fifteen,five,one,thirteen,twelve,seven,eleven,four,two,eight],
                                  [thirteen,seven,zero,nine,three,four,six,ten,two,eight,five,fourteen,twelve,eleven,fifteen,one],
                                  [thirteen,six,four,nine,eight,fifteen,three,zero,eleven,one,two,twelve,five,ten,fourteen,seven],
                                  [one,ten,thirteen,zero,six,nine,eight,seven,four,fifteen,fourteen,three,eleven,five,two,twelve]]);
        val S3 = Array2.fromList([[seven,thirteen,fourteen,three,zero,six,nine,ten,one,two,eight,five,eleven,twelve,four,fifteen],
                                  [thirteen,eight,eleven,five,six,fifteen,zero,three,four,seven,two,twelve,one,ten,fourteen,nine],
                                  [ten,six,nine,zero,twelve,eleven,seven,thirteen,fifteen,one,three,fourteen,five,two,eight,four],
                                  [three,fifteen,zero,six,ten,one,thirteen,eight,nine,four,five,eleven,twelve,seven,two,fourteen]]);
        val S4 = Array2.fromList([[two,twelve,four,one,seven,ten,eleven,six,eight,five,three,fifteen,thirteen,zero,fourteen,nine],
                                  [fourteen,eleven,two,twelve,four,seven,thirteen,one,five,zero,fifteen,ten,three,nine,eight,six],
                                  [four,two,one,eleven,ten,thirteen,seven,eight,fifteen,nine,twelve,five,six,three,zero,fourteen],
                                  [eleven,eight,twelve,seven,one,fourteen,two,thirteen,six,fifteen,zero,nine,ten,four,five,three]]);
        val S5 = Array2.fromList([[twelve,one,ten,fifteen,nine,two,six,eight,zero,thirteen,three,four,fourteen,seven,five,eleven],
                                  [ten,fifteen,four,two,seven,twelve,nine,five,six,one,thirteen,fourteen,zero,eleven,three,eight],
                                  [nine,fourteen,fifteen,five,two,eight,twelve,three,seven,zero,four,ten,one,thirteen,eleven,six],
                                  [four,three,two,twelve,nine,five,fifteen,ten,eleven,fourteen,one,seven,six,zero,eight,thirteen]]);
        val S6 = Array2.fromList([[four,eleven,two,fourteen,fifteen,zero,eight,thirteen,three,twelve,nine,seven,five,ten,six,one],
                                  [thirteen,zero,eleven,seven,four,nine,one,ten,fourteen,three,five,twelve,two,fifteen,eight,six],
                                  [one,four,eleven,thirteen,twelve,three,seven,fourteen,ten,fifteen,six,eight,zero,five,nine,two],
                                  [six,eleven,thirteen,eight,one,four,ten,seven,nine,five,zero,fifteen,fourteen,two,three,twelve]]);
        val S7 = Array2.fromList([[thirteen,two,eight,four,six,fifteen,eleven,one,ten,nine,three,fourteen,five,zero,twelve,seven],
                                  [one,fifteen,thirteen,eight,ten,three,seven,four,twelve,five,six,eleven,zero,fourteen,nine,two],
                                  [seven,eleven,four,one,nine,twelve,fourteen,two,zero,six,ten,thirteen,fifteen,three,five,eight],
                                  [two,one,fourteen,seven,four,ten,eight,thirteen,fifteen,twelve,nine,zero,three,five,six,eleven]]);
        (*  end S-boxes  *)

        fun str2bin "" = []
          | str2bin s  = let fun char2str c = StringCvt.padLeft #"0" 8 (Int.fmt StringCvt.BIN (ord c))(* converts a single char to eight bits in a string *)
                             fun char2int c = ord c mod 48                                            (* converts chars #"0" and #"1" to ints 0 and 1     *)
                         in
                             map char2int(explode(concat(map char2str(explode s))))
                         end;

        fun bin2str b = let fun bin2int bs = let val n = List.nth                                     (* converts a 8-int list to an int in 0-255         *)
                                             in
                                                 n(bs,0)*128 + n(bs,1)*64 + n(bs,2)*32 + n(bs,3)*16 + n(bs,4)*8 + n(bs,5)*4 + n(bs,6)*2 + n(bs,7) : int
                                             end
                            fun bin2char b =      if length(b) mod 8 <> 0 then raise Size
                                             else if length(b) = 0 then []
                                             else chr(bin2int(List.take(b,8))) :: bin2char(List.drop(b,8))
                        in
                            implode(bin2char b)
                        end;

        infix XOR;
        fun (x XOR y) = case x of 0 => (case y of 0 => 0 | 1 => 1 | _ => raise NotBinary)
                                | 1 => (case y of 0 => 1 | 1 => 0 | _ => raise NotBinary)
                                | _ => raise NotBinary

        infix xor;
        fun ([] xor [])           = []
          | ([] xor _ )           = raise Fail("Unequal length in A xor B, size A < size B")
          | ( _ xor [])           = raise Fail("Unequal length in A xor B, size A > size B")
          | ((x::xs) xor (y::ys)) = (x XOR y)::(xs xor ys);

        fun ls ([], _)    = []
          | ls (xs, 0)    = xs
          | ls (x::xs, n) = ls(xs @ [x], n-1);

        fun keys k = let fun PC1 xs = let val n = List.nth
                                      in ([n(xs,56),n(xs,48),n(xs,40),n(xs,32),n(xs,24),n(xs,16),n(xs, 8),
                                           n(xs, 0),n(xs,57),n(xs,49),n(xs,41),n(xs,33),n(xs,25),n(xs,17),
                                           n(xs, 9),n(xs, 1),n(xs,58),n(xs,50),n(xs,42),n(xs,34),n(xs,26),
                                           n(xs,18),n(xs,10),n(xs, 2),n(xs,59),n(xs,51),n(xs,43),n(xs,35)],
                                          [n(xs,62),n(xs,54),n(xs,46),n(xs,38),n(xs,30),n(xs,22),n(xs,14),
                                           n(xs, 6),n(xs,61),n(xs,53),n(xs,45),n(xs,37),n(xs,29),n(xs,21),
                                           n(xs,13),n(xs, 5),n(xs,60),n(xs,52),n(xs,44),n(xs,36),n(xs,28),
                                           n(xs,20),n(xs,12),n(xs, 4),n(xs,27),n(xs,19),n(xs,11),n(xs, 3)])
                                      end
                         fun PC2 xs = let val n = List.nth
                                      in
                                          [n(xs,13),n(xs,16),n(xs,10),n(xs,23),n(xs, 0),n(xs, 4),n(xs, 2),n(xs,27),
                                           n(xs,14),n(xs, 5),n(xs,20),n(xs, 9),n(xs,22),n(xs,18),n(xs,11),n(xs, 3),
                                           n(xs,25),n(xs, 7),n(xs,15),n(xs, 6),n(xs,26),n(xs,19),n(xs,12),n(xs, 1),
                                           n(xs,40),n(xs,51),n(xs,30),n(xs,36),n(xs,46),n(xs,54),n(xs,29),n(xs,39),
                                           n(xs,50),n(xs,44),n(xs,32),n(xs,47),n(xs,43),n(xs,48),n(xs,38),n(xs,55),
                                           n(xs,33),n(xs,52),n(xs,45),n(xs,41),n(xs,49),n(xs,35),n(xs,28),n(xs,31)]
                                      end
                         val   pc1 = PC1(k)
                         val  left = (#1 pc1)
                         val right = (#2 pc1)
                     in
                         [(PC2((ls(left, 1))@(ls(right, 1)))),
                          (PC2((ls(left, 2))@(ls(right, 2)))),
                          (PC2((ls(left, 4))@(ls(right, 4)))),
                          (PC2((ls(left, 6))@(ls(right, 6)))),
                          (PC2((ls(left, 8))@(ls(right, 8)))),
                          (PC2((ls(left,10))@(ls(right,10)))),
                          (PC2((ls(left,12))@(ls(right,12)))),
                          (PC2((ls(left,14))@(ls(right,14)))),
                          (PC2((ls(left,15))@(ls(right,15)))),
                          (PC2((ls(left,17))@(ls(right,17)))),
                          (PC2((ls(left,19))@(ls(right,19)))),
                          (PC2((ls(left,21))@(ls(right,21)))),
                          (PC2((ls(left,23))@(ls(right,23)))),
                          (PC2((ls(left,25))@(ls(right,25)))),
                          (PC2((ls(left,27))@(ls(right,27)))),
                          (PC2((ls(left, 0))@(ls(right, 0))))]
                     end;

        fun ip [] = []
          | ip xs = let val n = List.nth
                    in
                        [n(xs,57),n(xs,49),n(xs,41),n(xs,33),n(xs,25),n(xs,17),n(xs, 9),n(xs,1),
                         n(xs,59),n(xs,51),n(xs,43),n(xs,35),n(xs,27),n(xs,19),n(xs,11),n(xs,3),
                         n(xs,61),n(xs,53),n(xs,45),n(xs,37),n(xs,29),n(xs,21),n(xs,13),n(xs,5),
                         n(xs,63),n(xs,55),n(xs,47),n(xs,39),n(xs,31),n(xs,23),n(xs,15),n(xs,7),
                         n(xs,56),n(xs,48),n(xs,40),n(xs,32),n(xs,24),n(xs,16),n(xs, 8),n(xs,0),
                         n(xs,58),n(xs,50),n(xs,42),n(xs,34),n(xs,26),n(xs,18),n(xs,10),n(xs,2),
                         n(xs,60),n(xs,52),n(xs,44),n(xs,36),n(xs,28),n(xs,20),n(xs,12),n(xs,4),
                         n(xs,62),n(xs,54),n(xs,46),n(xs,38),n(xs,30),n(xs,22),n(xs,14),n(xs,6)]
                    end;

        fun ip' [] = []
          | ip' xs = let val n = List.nth
                     in
                         [n(xs,39),n(xs,7),n(xs,47),n(xs,15),n(xs,55),n(xs,23),n(xs,63),n(xs,31),
                          n(xs,38),n(xs,6),n(xs,46),n(xs,14),n(xs,54),n(xs,22),n(xs,62),n(xs,30),
                          n(xs,37),n(xs,5),n(xs,45),n(xs,13),n(xs,53),n(xs,21),n(xs,61),n(xs,29),
                          n(xs,36),n(xs,4),n(xs,44),n(xs,12),n(xs,52),n(xs,20),n(xs,60),n(xs,28),
                          n(xs,35),n(xs,3),n(xs,43),n(xs,11),n(xs,51),n(xs,19),n(xs,59),n(xs,27),
                          n(xs,34),n(xs,2),n(xs,42),n(xs,10),n(xs,50),n(xs,18),n(xs,58),n(xs,26),
                          n(xs,33),n(xs,1),n(xs,41),n(xs, 9),n(xs,49),n(xs,17),n(xs,57),n(xs,25),
                          n(xs,32),n(xs,0),n(xs,40),n(xs, 8),n(xs,48),n(xs,16),n(xs,56),n(xs,24)]
                     end;

        fun E xs = let val n = List.nth
                   in
                       [n(xs,31),n(xs, 0),n(xs, 1),n(xs, 2),n(xs, 3),n(xs, 4),
                        n(xs, 3),n(xs, 4),n(xs, 5),n(xs, 6),n(xs, 7),n(xs, 8),
                        n(xs, 7),n(xs, 8),n(xs, 9),n(xs,10),n(xs,11),n(xs,12),
                        n(xs,11),n(xs,12),n(xs,13),n(xs,14),n(xs,15),n(xs,16),
                        n(xs,15),n(xs,16),n(xs,17),n(xs,18),n(xs,19),n(xs,20),
                        n(xs,19),n(xs,20),n(xs,21),n(xs,22),n(xs,23),n(xs,24),
                        n(xs,23),n(xs,24),n(xs,25),n(xs,26),n(xs,27),n(xs,28),
                        n(xs,27),n(xs,28),n(xs,29),n(xs,30),n(xs,31),n(xs, 0)]
                   end;

        fun P xs = let val n = List.nth
                   in
                      [n(xs,15),n(xs, 6),n(xs,19),n(xs,20),n(xs,28),n(xs,11),n(xs,27),n(xs,16),
                       n(xs, 0),n(xs,14),n(xs,22),n(xs,25),n(xs, 4),n(xs,17),n(xs,30),n(xs, 9),
                       n(xs, 1),n(xs, 7),n(xs,23),n(xs,13),n(xs,31),n(xs,26),n(xs, 2),n(xs, 8),
                       n(xs,18),n(xs,12),n(xs,29),n(xs, 5),n(xs,21),n(xs,10),n(xs, 3),n(xs,24)]
                   end;

        fun F (p,k) = let fun lookup (box,bs) = let val n   = List.nth
                                                    val row = n(bs,0)*2 + n(bs,5)
                                                    val col = n(bs,1)*8 + n(bs,2)*4 + n(bs,3)*2 + n(bs,4)
                                                in
                                                    Array2.sub(box,row,col)
                                                end
                          fun sboxsub bs = let val G0 = (S0,List.take(bs,6))
                                               val G1 = (S1,List.take(List.drop(bs, 6),6))
                                               val G2 = (S2,List.take(List.drop(bs,12),6))
                                               val G3 = (S3,List.take(List.drop(bs,18),6))
                                               val G4 = (S4,List.take(List.drop(bs,24),6))
                                               val G5 = (S5,List.take(List.drop(bs,30),6))
                                               val G6 = (S6,List.take(List.drop(bs,36),6))
                                               val G7 = (S7,List.drop(bs,42))
                                           in
                                               (lookup G0) @ (lookup G1) @ (lookup G2) @ (lookup G3) @
                                               (lookup G4) @ (lookup G5) @ (lookup G6) @ (lookup G7)
                                           end
                      in
                          P(sboxsub(E(p) xor k))
                      end;

	fun crypt mode key text = let val ks          = if mode = "encrypt" then     keys (str2bin key) else
                                                        if mode = "decrypt" then rev(keys (str2bin key))
                                                                            else raise Fail("Illegal Mode Argument")
                                      fun swap bs     = ls (bs,32)
                                      fun round(n,bs) = let val left     = List.take(bs,32)
                                                            val right    = List.drop(bs,32)
	                                                    val roundkey = List.nth(ks,n)
                                                        in
                                                            right @ (left xor F(right,roundkey))
                                                        end
                                  in
                                      bin2str(ip'(swap(round(15,round(14,round(13,round(12,
                                                       round(11,round(10,round( 9,round( 8,
                                                       round( 7,round( 6,round( 5,round( 4,
                                                       round( 3,round( 2,round( 1,round( 0,ip(text))))))))))))))))))))
                                  end;

        fun ECBenc _ ""           = ""
          | ECBenc key plaintext  = let fun pad t = if (size(t) mod 8 = 0) then t
                                                    else pad (t^bin2str([0,0,0,0,0,0,1,1])) (* ASCII code 3 = End of Transmission *)
                                        val p = pad plaintext
                                        fun take l = implode(List.take(explode l,8))
                                        fun drop l = implode(List.drop(explode l,8))
                                    in
                                        (crypt "encrypt" key (str2bin(take p))) ^ (ECBenc key (drop p))
                                    end;

        fun ECBdec _ ""           = ""
          | ECBdec key ciphertext = if (size(ciphertext) mod 8 <> 0) then raise Size
                                    else let fun depad t = let fun delEOT [] = []
                                                                 | delEOT xs = if hd(xs) = chr(3) then delEOT (tl(xs))
                                                                                                  else xs
                                                           in
                                                               implode(rev(delEOT(rev(explode t))))
                                                           end
                                             fun take l = implode(List.take(explode l,8))
                                             fun drop l = implode(List.drop(explode l,8))
                                         in
                                                 depad((crypt "decrypt" key (str2bin(take ciphertext))) ^ (ECBdec key (drop ciphertext)))
                                         end;

        fun CBCenc key plaintext  = let val IV  = Random.rangelist (0,2) (64,Random.newgen())
                                        val cIV = crypt "encrypt" key IV
                                        fun enc _ "" _             = ""
                                          | enc key ptxt prev = if size(ptxt) < 8 then let fun pad l = if (List.length(l) mod 64 = 0) then l
                                                                                                       else pad (l @ [0,0,0,0,0,0,1,1])
                                                                                           val p = pad (str2bin ptxt)
                                                                                       in
                                                                                           crypt "encrypt" key (prev xor p)
                                                                                       end
                                                                else let val curr = crypt "encrypt" key (prev xor (List.take(str2bin ptxt,64)))
                                                                     in
                                                                         curr ^ enc key (implode(List.drop(explode ptxt,8))) (str2bin curr)
                                                                     end
                                    in
                                        cIV ^ (enc key plaintext IV)
                                    end;

        fun CBCdec key ciphertext = let val IV = crypt "decrypt" key (List.take(str2bin(ciphertext),64))
                                        fun dec _ "" _        = ""
                                          | dec key ctxt prev = let val curr = List.take(str2bin ctxt,64)
                                                                    fun depad t = let fun delEOT [] = []
                                                                                        | delEOT xs = if hd(xs) = chr(3) then delEOT (tl(xs))
                                                                                                      else xs
                                                           in
                                                               implode(rev(delEOT(rev(explode t))))
                                                           end
                                                                in
                                                                   depad(bin2str(prev xor str2bin(crypt "decrypt" key (curr))) ^ dec key (implode(List.drop(explode ctxt,8))) curr)
                                                                end
                                    in
                                        dec key (implode(List.drop(explode ciphertext,8))) (str2bin IV)
                                    end;

        fun TrDESenc keys plaintext  = let val E1 = ECBenc (#1 keys)
                                           val  D = ECBdec (#2 keys)
                                           val E2 = ECBenc (#3 keys)
                                       in
                                          (E2 o D o E1) plaintext
                                       end;

        fun TrDESdec keys ciphertext = let val D1 = ECBdec (#3 keys)
                                           val  E = ECBenc (#2 keys)
                                           val D2 = ECBdec (#1 keys)
                                       in
                                          (D2 o E o D1) ciphertext
                                       end;
    end;

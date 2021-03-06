open Constants
open Update
open Monster
open Dijkstra
open Player
(*open Display*)

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                      CONSTANTS  TESTS                                ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

let get = function
  |Some x -> x
  |_ -> failwith "None"

TEST "level 0" =
  Constants.get_master 0 =

               [[false;false;false;false;true;false;false;false;false];
                [false;false;false;false;true;false;false;false;false];
                [false;false;false;false;true;false;false;false;false];
                [false;false;false;false;true;false;false;false;false];
                [true ;true ;true ;true ;true;true ;true ;true ;true ];
                [false;false;false;false;true;false;false;false;false];
                [false;false;false;false;true;false;false;false;false];
                [false;false;false;false;true;false;false;false;false];
                [false;false;false;false;true;false;false;false;false]]
  && Constants.get_weights 0 =

               [[1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1.   ;   1.;   1.;  1. ;1.;  1. ;  1. ;  1. ;  1. ];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.];
                [1000.;1000.;1000.;1000.;1.;1000.;1000.;1000.;1000.]]
  && Constants.init_level 0 =

                  {previous = None;
                  level_number = 0; game_progress = Unstarted;
                  player_position = (4, 4);
                  monster_position = [(Up, (6, 4)); (Right, (4, 0))];
                  time = 20}


(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                      DJIKSTRA  TEST                                  ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)
(* test Dijkstra *)
TEST "Djikstra: straight line" =
  Dijkstra.dijkstra (4,0) (4,4) (get (Some (Constants.get_weights 0))) = (4,1)

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                      MONSTER TESTS                                   ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

(* test movement of all types of monsters *)
TEST "Monster: up" =
Monster.update_monster_position
(Up,(2,2)) (0,0) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Up,(1,2))

TEST "Monster: up switch" =
Monster.update_monster_position
(Up,(0,0)) (2,2) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Down,(0,0))

TEST "Monster: down" =
Monster.update_monster_position
(Down,(0,0)) (2,2) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Down,(1,0))

TEST "Monster: down switch" =
Monster.update_monster_position
(Down,(2,2)) (0,0) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Up,(2,2))

TEST "Monster: left" =
Monster.update_monster_position
(Left,(2,2)) (0,0) (get (Some(Constants.get_master (-2))))
(get (Some(Constants.get_weights (-2)))) =  (Left,(2,1))

TEST "Monster: left switch" =
Monster.update_monster_position
(Left,(0,0)) (2,2) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Right,(0,0))

TEST "Monster: right" =
Monster.update_monster_position
(Right,(0,0)) (2,2) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Right,(0,1))

TEST "Monster: right switch" =
Monster.update_monster_position
(Right,(2,2)) (0,0) (get (Some(Constants.get_master (-2))))
(get(Some(Constants.get_weights (-2)))) =  (Left,(2,2))

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                     PLAYER  TEST                                     ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

(* test movement of player- reading the keys correctly *)
TEST "PLayer: up down left right" =
  Player.update_player_position (4,4) ['w']
                                (get (Some(Constants.get_master 0))) = (3,4) &&
  Player.update_player_position (4,4) ['a']
                                (get (Some(Constants.get_master 0))) = (4,3) &&
  Player.update_player_position (4,4) ['s']
                                (get (Some(Constants.get_master 0)))  = (5,4) &&
  Player.update_player_position (4,4) ['d']
                                (get (Some(Constants.get_master 0))) = (4,5)

(* test collision detection *)
TEST "PLayer: collisions" =
  Player.update_player_position (4,1) ['w']
                                (get (Some(Constants.get_master 0))) = (4,1) &&
  Player.update_player_position (3,4) ['a']
                                (get (Some(Constants.get_master 0)))  = (3,4) &&
  Player.update_player_position (4,6) ['s']
                                (get (Some(Constants.get_master 0))) = (4,6) &&
  Player.update_player_position (8,4) ['d']
                                (get (Some(Constants.get_master 0)))  = (8,4)


let () = Pa_ounit_lib.Runtime.summarize()

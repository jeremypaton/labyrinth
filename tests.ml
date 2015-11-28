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
               Some
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
               Some
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
   Some
                  {level_number = 0; game_progress = Unstarted;
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

TEST "Djikstra: straight line" =
  Dijkstra.dijkstra (4,0) (4,4) (get (Constants.get_weights 0)) = (4,1)

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                      MONSTER TESTS                                   ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

TEST "Monster: up" =
Monster.update_monster_position
(Up,(2,2)) (0,0) (get (Constants.get_master (-2))) (get(Constants.get_weights (-1))) =  (Up,(2,3))


(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                     PLAYER  TEST                                     ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

TEST "PLayer: up down left right" =
  Player.update_player_position (4,4) ['w'] (get (Constants.get_master 0)) = (3,4) &&
  Player.update_player_position (4,4) ['a'] (get (Constants.get_master 0)) = (4,3) &&
  Player.update_player_position (4,4) ['s'] (get (Constants.get_master 0))  = (5,4) &&
  Player.update_player_position (4,4) ['d'] (get (Constants.get_master 0)) = (4,5)

TEST "PLayer: collisions" =
  Player.update_player_position (4,1) ['w'] (get (Constants.get_master 0)) = (4,1) &&
  Player.update_player_position (3,4) ['a'] (get (Constants.get_master 0))  = (3,4) &&
  Player.update_player_position (4,6) ['s'] (get (Constants.get_master 0)) = (4,6) &&
  Player.update_player_position (8,4) ['d'] (get (Constants.get_master 0))  = (8,4)

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                    UPDATE TESTS                                      ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

(******************************************************************************
 ******************************************************************************
 ****                                                                      ****
 ****                     DISPLAY TEST                                     ****
 ****                                                                      ****
 ******************************************************************************
 ******************************************************************************)

let () = Pa_ounit_lib.Runtime.summarize()

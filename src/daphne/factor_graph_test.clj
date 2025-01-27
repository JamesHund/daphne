(ns daphne.factor-graph-test
  (:require
   [clojure.test :refer [deftest is run-tests]]
   [daphne.factor-graph :refer [remove-cruft]]))

(def factor-graph
  [{}
   {:X ['team_1_3 'perf_2284 'perf_646 'perf_9255 'skill_10168 'perf_9278 'd1 'team_1_2 'skill_10167
        'perf_9257 'team_1 'skill_9278 'team_1_1 'perf_10190 'skill_10190 'skill_2284 'perf_10169
        'team_2_2 'team_2 'team_2_3 'perf_44 'skill_44 'skill_9255 'perf_10168 'skill_10169
        'skill_646 'team_2_1 'perf_10167 'skill_9257],
    :F ['f-team_1_3 'f-perf_2284 'f-perf_646 'f-perf_9255 'f-skill_10168 'f-perf_9278 'f-d1
        'f-team_1_2 'f-w1 'f-skill_10167 'f-perf_9257 'f-team_1 'f-skill_9278 'f-team_1_1
        'f-perf_10190 'f-skill_10190 'f-skill_2284 'f-perf_10169 'f-team_2_2 'f-team_2
        'f-team_2_3 'f-perf_44 'f-skill_44 'f-skill_9255 'f-perf_10168 'f-skill_10169
        'f-skill_646 'f-team_2_1 'f-perf_10167 'f-skill_9257],
    :A {'f-team_2 ['perf_646 'perf_9257 'team_2],
        'f-perf_10190 ['perf_10190 'skill_10190],
        'f-team_1_3 ['team_1_3 'team_1_2 'perf_10169],
        'f-skill_9257 ['skill_9257],
        'f-team_2_1 ['perf_9255 'team_2 'team_2_1],
        'f-perf_10168 ['skill_10168 'perf_10168],
        'f-perf_9255 ['perf_9255 'skill_9255],
        'f-skill_646 ['skill_646],
        'f-perf_2284 ['perf_2284 'skill_2284],
        'f-team_1 ['perf_2284 'team_1 'perf_10168],
        'f-team_1_1 ['team_1 'team_1_1 'perf_10190],
        'f-skill_10190 ['skill_10190],
        'f-d1 ['d1 'team_2_3],
        'f-perf_10167 ['skill_10167 'perf_10167],
        'f-skill_2284 ['skill_2284],
        'f-w1 ['d1],
        'f-perf_44 ['perf_44 'skill_44],
        'f-skill_9278 ['skill_9278],
        'f-perf_9257 ['perf_9257 'skill_9257],
        'f-skill_10169 ['skill_10169],
        'f-skill_44 ['skill_44],
        'f-skill_10168 ['skill_10168],
        'f-team_2_3 ['team_2_2 'team_2_3 'perf_44],
        'f-perf_9278 ['perf_9278 'skill_9278],
        'f-team_2_2 ['perf_9278 'team_2_2 'team_2_1],
        'f-perf_10169 ['perf_10169 'skill_10169],
        'f-perf_646 ['perf_646 'skill_646],
        'f-skill_9255 ['skill_9255],
        'f-team_1_2 ['team_1_2 'team_1_1 'perf_10167],
        'f-skill_10167 ['skill_10167]},
    :psi {'f-team_2 ['dirac 'team_2 ['+' 'perf_9257 'perf_646]],
          'f-perf_10190 ['normal 'perf_10190 'skill_10190 1.0],
          'f-team_1_3 ['dirac 'team_1_3 ['+' 'perf_10169 'team_1_2]],
          'f-skill_9257 ['normal 'skill_9257 24.06884600364015 8.333],
          'f-team_2_1 ['dirac 'team_2_1 ['+' 'perf_9255 'team_2]],
          'f-perf_10168 ['normal 'perf_10168 'skill_10168 1.0],
          'f-perf_9255 ['normal 'perf_9255 'skill_9255 1.0],
          'f-skill_646 ['normal 'skill_646 27.849622826265083 8.333],
          'f-perf_2284 ['normal 'perf_2284 'skill_2284 1.0],
          'f-team_1 ['dirac 'team_1 ['+' 'perf_10168 'perf_2284]],
          'f-team_1_1 ['dirac 'team_1_1 ['+' 'perf_10190 'team_1]],
          'f-skill_10190 ['normal 'skill_10190 27.494090574133864 8.333],
          'f-d1 ['dirac 'd1 ['-' 'team1 'team_2_3]],  ;; 'team1' appears here
          'f-perf_10167 ['normal 'perf_10167 'skill_10167 1.0],
          'f-skill_2284 ['normal 'skill_2284 28.400782522129198 8.333],
          'f-w1 ['dirac true ['absleq 'd1 0.0]],
          'f-perf_44 ['normal 'perf_44 'skill_44 1.0],
          'f-skill_9278 ['normal 'skill_9278 29.041405427585378 8.333],
          'f-perf_9257 ['normal 'perf_9257 'skill_9257 1.0],
          'f-skill_10169 ['normal 'skill_10169 24.442252124753296 8.333],
          'f-skill_44 ['normal 'skill_44 29.049396369265235 8.333],
          'f-skill_10168 ['normal 'skill_10168 24.881061475135606 8.333],
          'f-team_2_3 ['dirac 'team_2_3 ['+' 'perf_44 'team_2_2]],
          'f-perf_9278 ['normal 'perf_9278 'skill_9278 1.0],
          'f-team_2_2 ['dirac 'team_2_2 ['+' 'perf_9278 'team_2_1]],
          'f-perf_10169 ['normal 'perf_10169 'skill_10169 1.0],
          'f-perf_646 ['normal 'perf_646 'skill_646 1.0],
          'f-skill_9255 ['normal 'skill_9255 25.012020179812012 8.333],
          'f-team_1_2 ['dirac 'team_1_2 ['+' 'perf_10167 'team_1_1]],
          'f-skill_10167 ['normal 'skill_10167 21.453513640735952 8.333]}}
   ['vector' 'skill_10169' 'skill_10167' 'skill_10190' 'skill_10168' 'skill_2284'
    'skill_44' 'skill_9278' 'skill_9255' 'skill_9257' 'skill_646']])

;; Define the test
(deftest test-remove-cruft-f-d1-includes-team1
  (let [result-updated (remove-cruft factor-graph)
        adjacency-list-updated (:A (second result-updated))]
    (is (contains? (set (adjacency-list-updated 'f-d1)) 'team1)
        "f-d1 should include team1 in the adjacency list")))

;; Run the test
(run-tests 'daphne.factor-graph-test)

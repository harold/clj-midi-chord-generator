(ns clj-midi-chord-generator.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell])
  (:import [java.io File]
           [javax.sound.midi Sequence ShortMessage MidiEvent MidiSystem])
  (:gen-class))

(def version "0.1")

(def qualities [{:name " major" :intervals [0 4 7]}
                {:name " dim" :intervals [0 3 6]}
                {:name " augmented" :intervals [0 4 8]}
                {:name "sus4" :intervals [0 5 7]}
                {:name "6" :intervals [0 4 7 9]}
                {:name "7" :intervals [0 4 7 10]}
                {:name "m7(b5)" :intervals [0 3 6 10]}
                {:name "dim7" :intervals [0 3 6 9]}
                {:name "7b9" :intervals [0 4 7 10 13]}
                {:name "7#9" :intervals [0 4 7 10 15]}
                {:name "9" :intervals [0 4 7 10 14]}
                {:name "9sus4" :intervals [0 5 7 10 14]}
                {:name "9b5" :intervals [0 4 6 10 14]}
                {:name "9#5" :intervals [0 4 8 10 14]}
                {:name "9#11" :intervals [0 4 7 10 18]}
                {:name "13" :intervals [0 4 7 10 14 21]}
                {:name "13sus4" :intervals [0 5 7 10 14 21]}
                {:name "13b5" :intervals [0 4 6 10 14 21]}
                {:name "13#5" :intervals [0 4 8 10 14 21]}
                {:name "13b9" :intervals [0 4 7 10 13 21]}
                {:name "13#9" :intervals [0 4 7 10 15 21]}
                {:name "13b5b9" :intervals [0 4 6 10 13 21]}
                {:name "13b5#9" :intervals [0 4 6 10 15 21]}
                {:name "13#5b9" :intervals [0 4 8 10 13 21]}
                {:name "13#5#9" :intervals [0 4 8 10 15 21]}
                {:name " major7" :intervals [0 4 7 11]}
                {:name " 6-9" :intervals [0 4 7 9 14]}
                {:name " major9" :intervals [0 4 7 11 14]}
                {:name " major9#11" :intervals [0 4 7 11 14 18]}
                {:name " major13" :intervals [0 4 7 11 14 21]}
                {:name " major13b5" :intervals [0 4 6 11 14 21]}
                {:name " major13#5" :intervals [0 4 8 11 14 21]}
                {:name " major13b9" :intervals [0 4 7 11 13 21]}
                {:name " major13#9" :intervals [0 4 7 11 15 21]}
                {:name " major13b5b9" :intervals [0 4 6 11 13 21]}
                {:name " major13b5#9" :intervals [0 4 6 11 15 21]}
                {:name " major13#5b9" :intervals [0 4 8 11 13 21]}
                {:name " major13#5#9" :intervals [0 4 8 11 15 21]}
                {:name " minor6" :intervals [0 3 7 9]}
                {:name " minor7" :intervals [0 3 7 10]}
                {:name " minor7b9" :intervals [0 3 7 10 13]}
                {:name " minor9" :intervals [0 3 7 10 14]}
                {:name " minor11" :intervals [0 3 7 10 14 17]}
                {:name " minor13" :intervals [0 3 7 10 14 21]}
                {:name " minor (major7)" :intervals [0 3 7 11]}
                {:name " minor9 (major7)" :intervals [0 3 7 11 14]}])

(def roots [{:name "C" :offset 0}
            {:name "C# (Db)" :offset 1}
            {:name "D" :offset 2}
            {:name "D# (Eb)" :offset 3}
            {:name "E" :offset 4}
            {:name "F" :offset 5}
            {:name "F# (Gb)" :offset 6}
            {:name "G" :offset 7}
            {:name "G# (Ab)" :offset 8}
            {:name "A" :offset 9}
            {:name "A# (Bb)" :offset 10}
            {:name "B" :offset 11}])

(def scales [{:name "Major" :intervals [0 2 4 5 7 9 11]}
             {:name "minor" :intervals [0 2 3 5 7 8 10]}
             {:name "harmonic minor" :intervals [0 2 3 5 7 8 11]}])

(defn- chord-instances
  [root quality]
  (loop [out []
         notes (vec (map #(+ (:offset root) %) (:intervals quality)))]
    (if (every? #(<= 0 % 127) notes)
      (recur (conj out notes)
             (if (> 12 (-> quality :intervals last))
               (vec (sort (concat [(+ 12 (nth notes 0))]
                                  (rest notes))))
               (mapv #(+ 12 %) notes)))
      out)))

(defn- chord-near-middle-c
  [root quality]
  (let [instances (chord-instances root quality)
        distance (fn [notes] (apply + (map #(Math/abs (- % 60)) notes)))]
    {:name (format "%s%s" (:name root) (:name quality))
     :offset (:offset root)
     :notes (apply min-key distance instances)}))

(defn- every-chord-near-middle-c
  []
  (for [root roots
        quality qualities]
    (chord-near-middle-c root quality)))

(defn- root-scale->note-set
  [root scale]
  (->> (for [octave (range 20)]
         (->> (:intervals scale)
              (map #(+ (* 12 octave) (:offset root) %))
              (filter #(<= 0 % 127))))
       (apply concat)
       (set)))

(defn- add-note-to-track!
  [track note]
  (let [note-on-message (ShortMessage. 0x90 note 0x60)
        note-on-midi-event (MidiEvent. note-on-message 0)
        note-off-message (ShortMessage. 0x80 note 0x40)
        note-off-midi-event (MidiEvent. note-off-message 96)]
    (.add track note-on-midi-event)
    (.add track note-off-midi-event)))

(defn -main
  [& args]
  (let [chords (every-chord-near-middle-c)
        base-path (format "./midi-chords-v%s" version)]
    (shell/sh "rm" "-rf" base-path)
    (doseq [root roots
            [scale-index scale] (map-indexed vector scales)]
      (let [note-set (root-scale->note-set root scale)]
        (doseq [chord (filter #(every? note-set (:notes %)) chords)]
          (let [sequence (Sequence. Sequence/PPQ 24)
                track (.createTrack sequence)
                scale-degree (inc (.indexOf (:intervals scale)
                                            (mod (- (:offset chord) (:offset root)) 12)))
                path (format "%s/%02d.%s - %s %s/%02d - %s.mid" base-path
                             (:offset root) scale-index
                             (:name root) (:name scale) scale-degree (:name chord))]
            (doseq [note (:notes chord)]
              (add-note-to-track! track note))
            (io/make-parents path)
            (println path)
            (MidiSystem/write sequence 1 (File. path)))))))
  :ok)

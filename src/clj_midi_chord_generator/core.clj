(ns clj-midi-chord-generator.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell])
  (:import [java.io File]
           [javax.sound.midi Sequence ShortMessage MidiEvent MidiSystem])
  (:gen-class))

(def version 0)

(def qualities [{:name "Maj" :intervals [0 4 7]}
                {:name "min" :intervals [0 3 7]}
                {:name "dim" :intervals [0 3 6]}
                {:name "aug" :intervals [0 4 8]}
                {:name "sus4" :intervals [0 5 7]}])

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
             {:name "minor" :intervals [0 2 3 5 7 8 10]}])

(defn- chord-near-middle-c
  [root quality]
  (let [instances (loop [out []
                         notes (vec (map #(+ (:offset root) %) (:intervals quality)))]
                    (if (every? #(<= 0 % 127) notes)
                      (recur (conj out notes)
                             (vec (sort (concat [(+ 12 (nth notes 0))]
                                                (rest notes)))))
                      out))
        distance (fn [notes] (apply + (map #(Math/abs (- % 60)) notes)))]
    {:name (format "%s %s" (:name root) (:name quality))
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
            scale scales]
      (let [note-set (root-scale->note-set root scale)]
        (doseq [chord (filter #(every? note-set (:notes %)) chords)]
          (let [sequence (Sequence. Sequence/PPQ 24)
                track (.createTrack sequence)
                scale-degree (inc (.indexOf (:intervals scale)
                                            (mod (- (:offset chord) (:offset root)) 12)))
                path (format "%s/%02d - %s %s/%02d - %s.mid" base-path
                             (:offset root) (:name root) (:name scale) scale-degree (:name chord))]
            (doseq [note (:notes chord)]
              (add-note-to-track! track note))
            (io/make-parents path)
            (println path)
            (MidiSystem/write sequence 1 (File. path)))))))
  :ok)

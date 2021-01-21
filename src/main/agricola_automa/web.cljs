(ns agricola-automa.web
  (:require [agricola-automa.ui :as ui]
            [rum.core :as rum]))

(defn ^:dev/after-load start!
  []
  (rum/mount (ui/ui-game)
             (js/document.getElementById "app")))
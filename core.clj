(ns walletsystem.core
  (:require [clojure.pprint :as p]))

(defrecord wallet[fullName amount statement transactions createdOn fdAmount fdIter])

(def wallets (atom {}))
(def createdOn 0)

(defn createNewWallet[fullName amount]
  (def createdOn (inc createdOn))
  (wallet. fullName amount "" 0 createdOn -1 -1))

(defn addNewWallet [fullName amount walletList]
  (if (< (read-string amount) 0) (do (println "Amount cannot be negative") walletList)
  (assoc walletList (keyword fullName) (createNewWallet fullName (read-string amount)))))

(defn transfer [walletList name1 name2 amount]
  (if (or (< (- (:amount (name1 walletList)) amount) 0) (< amount 0.0001)) walletList
  (let [  fdBonus (if (= 0 (:fdIter (name1 walletList))) 10 0)
          bonus (if (= (- (:amount (name1 walletList)) amount) (+ (:amount (name2 walletList)) amount)) 10 0)
          bonusString (if (not (= bonus 0)) "\nOffer1 credit 10" "")]
    (assoc walletList name1 (assoc (name1 walletList)
                                      :transactions (+ 1 (:transactions (name1 walletList)))
                                      :amount (+ fdBonus bonus (- (:amount (name1 walletList)) amount))
                                      :fdIter (cond
                                                (= -1 (:fdIter (name1 walletList))) -1
                                                (= 0 (:fdIter (name1 walletList))) -1
                                                (< (:amount (name1 walletList)) (read-string (:fdAmount (name1 walletList)))) -1 
                                                :else (dec (:fdIter (name1 walletList))))
                                      :statement (str (:statement (name1 walletList)) "\n" name2 " debit " amount bonusString (if (= (:fdIter (name1 walletList)) -1)   "" "\nFDBonus: 10"))
                                      )
                      name2 (assoc (name2 walletList) 
                                      :transactions (+ 1 (:transactions (name2 walletList)))
                                      :amount (+ bonus (:amount (name2 walletList)) amount)
                                      :statement (str (:statement (name1 walletList)) "\n" name1 " credit " amount bonusString))))))

(defn fixedDeposit[walletList name fdAmount]
  (assoc walletList (keyword name) (assoc ((keyword name) walletList) :fdAmount fdAmount :fdIter 5)))
        
(defn statement [walletList name]
  (println (:statement (name walletList))))

(defn overview [walletList]
  (println "Overview")
  (doseq [w walletList]
    (println (:fullName (second w)) " " (:amount (second w)))))

(defn compartor [k1 k2]
  (if (= (:transactions (second k2)) (:transactions (second k1))) 
    (if (= (:amount (second k1)) (:amount (second k2))) 
        (compare (:createdOn (second k1)) (:createdOn (second k2)))
      (compare (:amount (second k2)) (:amount (second k1))))
    (compare (:transactions (second k2)) (:transactions (second k1)))))

(defn applyOffer2 [walletList]
  (doseq [[x y] (map list (take 3 (sort compartor @wallets)) (list 10 5 2))] 
      (reset! wallets (assoc @wallets (first x) (assoc ((first x) @wallets) 
      :statement (str (:statement ((first x) @wallets)) "\nOffer2")
      :amount (+ y (:amount ((first x) @wallets))))))))

(defn processString [s]
  (doseq [x (clojure.string/split-lines s)]
    (let [ll (clojure.string/split x #" ")]
      (cond
        (= (first ll) "CreateWallet") (reset! wallets (addNewWallet (read-string (second ll)) (last ll) @wallets))
        (= (first ll) "Overview") (overview @wallets)
        (= (first ll) "TransferMoney") (reset! wallets (transfer @wallets (keyword (second ll)) (keyword (second (rest ll))) (read-string (last ll)) ))
        (= (first ll) "Statement") (do (print "Statement for: " (second ll)) (statement @wallets (keyword (second ll))))
        (= (first ll) "Offer2") (applyOffer2 @wallets)
        :else "Wrong action")
      )))

; (println "----------------")
(def actionString "CreateWallet Harry 100\nCreateWallet Ron 95.7\nCreateWallet Hermione 104\nCreateWallet Albus 200\nCreateWallet Draco 500\nOverview\nTransferMoney Albus Draco 30\nTransferMoney Hermione Harry 2\nTransferMoney Albus Ron 5\nOverview\nStatement Harry\nOffer2\nOverview\n")
; (println "----------------")
; (def actionString "CreateWallet Harry 100\nCreateWallet Tom 104\nTransferMoney Tom Harry 2\nStatement Tom\nOverview")

; (println "\n\n\n\n\n\n\n\n\n\n\n")
(processString actionString)
; (applyOffer2 wallets)
; (overview wallets)

; (def wallets (addNewWallet "Ron" "100" wallets))
; (def wallets (fixedDeposit wallets "Ron" "100"))
; (def wallets (addNewWallet "Harry" "100" wallets))
; (def wallets (transfer wallets :Ron :Harry 2))
; (def wallets (transfer wallets :Ron :Harry 2))
; (def wallets (transfer wallets :Ron :Harry 2))
; (def wallets (transfer wallets :Ron :Harry 2))
; (def wallets (transfer wallets :Ron :Harry 2))
; (statement wallets :Ron)

; (overview wallets)

; (println (:Ron wallets))
"Done"
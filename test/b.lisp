(eval-when (:compile-toplevel)
  (print '(:compile :b)))
(eval-when (:load-toplevel)
  (print '(:load :b)))
(eval-when (:execute)
  (print '(:execute :b)))

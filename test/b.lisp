(eval-when (:compile-toplevel)
  (print '(:compile :b))
  (finish-output))
(eval-when (:load-toplevel)
  (print '(:load :b))
  (finish-output))
(eval-when (:execute)
  (print '(:execute :b))
  (finish-output))

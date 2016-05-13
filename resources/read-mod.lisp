(defvar *psi-mod-obo* )
(defun read-psi-mod (&key (path (merge-pathnames "PSI_MOD.obo" *load-pathname*)))
  (let ((instance (make-instance 'obo :path path)))
    (setq *psi-mod-obo* instance)
    (read-obo instance)))

  

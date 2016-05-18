(defvar *this-file-path* *load-truename*)
(defvar *ontology-root-path* *this-file-path*)

(defmacro with-ptm-terms (&body body)
  `(let ((is-about !obo:IAO_0000136)
	 (location-description !obo:PROXXX_0001003)
	 (protein-site !obo:PROXXX_0001000)
	 (part-of !obo:BFO_0000177)
	 (site-of-ptm !obo:PROXXX_0001002)
	 (ptmed-protein !obo:PROXXX_0001019))
     ,@body))


(defun query-ptmed-protein ()
  (let ((o (load-ontology (namestring (merge-pathnames "histone.owl" *ontology-root-path*)))))
    (with-ptm-terms
	(sparql `(:select (?label) () 
			  (?protein_class !rdfs:subClassOf ,ptmed-protein)
			  (?protein_class !rdfs:label ?label)
			  (:filter (not(equal ?protein_class ,ptmed-protein))))
		:kb o :trace t))))

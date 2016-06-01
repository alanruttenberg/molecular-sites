(defvar *this-file-path* *load-truename*)
(defvar *ontology-root-path* *this-file-path*)

(defvar *ontology*)

(defmacro with-ptm-terms (&body body)
  `(let ((is-about !obo:IAO_0000136)
	 (location-description !obo:PROXXX_0001003)
	 (protein-site !obo:PROXXX_0001000)
	 (part-of !obo:BFO_0000177)
	 (site-of-ptm !obo:PROXXX_0001002)
	 (ptmed-protein !obo:PROXXX_0001019)
	 (mouse-histone-h3.3 !obo:PR_P84244)
	 (has-site !obo:PROXXX_0001019)
	 (is-occupied-by !obo:PROXXX_0001011)
	 (modified-residue !obo:PROXXX_0001023)
	 (site-of-residue !obo:PROXXX_0001001)
	 (described-by !obo:PROXXX_0001021)
	 (has-ordinal-position !obo:PROXXX_204))
     ,@body))


(defun query-ptmed-protein ()
  (unless (boundp '*ontology*)
    (setq *ontology* (load-ontology (namestring (merge-pathnames "histone.owl" *ontology-root-path*)))))
  (with-ptm-terms
    (sparql `(:select (?label) () 
		      (?protein_class !rdfs:subClassOf ,ptmed-protein)
		      (?protein_class !rdfs:label ?label)
		      (:filter (not(equal ?protein_class ,ptmed-protein))))
	    :kb *ontology* :trace t)))

(defun query-protein-modified-forms ()
  (unless (boundp '*ontology*)
    (setq *ontology* (load-ontology (namestring (merge-pathnames "histone.owl" *ontology-root-path*)))))
  (with-ptm-terms
    (sparql `(:select (?label) () 
		      (?subclass !rdfs:subClassOf ,mouse-histone-h3.3)
		      (?subclass !rdfs:subClassOf ,ptmed-protein)
		      (?subclass !rdfs:label ?label)
		      )
	    :kb *ontology* :trace t)))
  
(defun describe-protein-modified-sites ()
  (unless (boundp '*ontology*)
    (setq *ontology* (load-ontology (namestring (merge-pathnames "histone.owl" *ontology-root-path*)))))
  (with-ptm-terms
    (sparql `(:select (?label ?position) () 
		      (?site !rdfs:subClassOf ,site-of-residue)
		      (?site !rdfs:subClassOf ?restriction)
		      (?restriction !rdf:type !owl:Restriction)
		      (?restriction !owl:onProperty ,part-of)
		      (?restriction !owl:someValuesFrom ,mouse-histone-h3.3)
		      (?site !rdfs:subClassOf ?restriction2)
		      (?restriction2 !rdf:type !owl:Restriction)
		      (?restriction2 !owl:onProperty ,is-occupied-by)
		      (?restriction2 !owl:someValuesFrom ,modified-residue)
		      (?site !rdfs:label ?label)
 		      (:optional
		       (?site !rdfs:subClassOf ?restriction3)
		       (?restriction3 !rdf:type !owl:Restriction)
		       (?restriction3 !owl:onProperty ,described-by)
		       (?position !rdf:type !owl:ObjectProperty)
		       (?restriction3 !owl:hasValue ?position)
					;(?position ,has-ordinal-position ?num)
		       ))
  	    :kb *ontology* :trace t)))




  
  

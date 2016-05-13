(defun read-resid (&optional (path "~/repos/pro-ontology/trunk/src/ontology/protein-sites/RESIDUES.XML"))
  (with-open-file (f path) 
    (xmls:parse f)))

(defun each-resid-entry (resid fn)
  (loop for entry in (find-elements-with-tag resid "Entry")
     for id = (attribute-named entry "id")
     for iri = (make-uri nil (#"replaceFirst" id "AA(.*)" "obo:RESID_$1"))
     for names = (find-element-with-tag entry "Names")
     for label = (format nil "~a residue" (third (find-element-with-tag names "Name")))
     for synonyms = (mapcar (lambda(n) (format nil "~a residue" n)) 
			    (mapcar 'third (find-elements-with-tag names "AlternateName")))
     for scode = (find-element-with-tag entry "SequenceCode")
     for 1-letter-code = (third (find-element-with-tag scode "SequenceSpec"))
     for 3-letter-code = (third (find-element-with-tag scode "Abbreviation"))
     for deprecated = (equal (third (find-element-with-tag entry "Source")) "deprecated")
     for cross-linked = (parse-integer 
			 (or (subseq (find "cross-link" (mapcar 'third (find-elements-with-tag scode "Condition"))
				       :test (lambda(s e) (search s e :test 'char=))) 11) "0"))
     for mod-accession = (mapcar (lambda(e) (subseq e 4)) 
				 (remove-if-not (lambda(e) (search "PSI" e :test 'char=))
						(mapcar 'third (find-elements-with-tag entry "SequenceCode" "Xref"))))

     for formula =  (third (find-element-with-tag entry "FormulaBlock" "Formula"))
     for definition-sources = (mapcar 'third (find-elements-with-tag entry "Citation"))
     do
       (funcall fn :entry entry :id id :iri iri :label label :synonyms synonyms :1-letter-code 
		1-letter-code :3-letter-code 3-letter-code :mod-accession mod-accession
		:definition-sources definition-sources :formula formula :cross-linked cross-linked :deprecated deprecated)))

(make-instance 'label-source :key :iao :sources (list (uri-full !obo:iao/bfo2-transitional/ontology-metadata.owl)))

(defun create-resid-ontology ()
  (let ((all-resids nil))
    (with-ontology resid (:collecting t :ontology-iri "http://purl.obolibrary.org/obo/resid.owl")
	((asq
	  (declaration (annotation-property !'editor preferred term'@iao))
	  (declaration (annotation-property !'definition source'@iao))
	  (declaration (annotation-property !'definition'@iao))
	  (declaration (annotation-property !'alternative term'@iao))
	  (declaration (annotation-property !'term editor'@iao))
	  (declaration (annotation-property !obo:PROXXX_200))
	  (annotation-assertion !rdfs:label !obo:PROXXX_200 "PSI-MOD accession")
	  (sub-annotation-property-of !obo:PROXXX_200 !'alternative term'@iao)
	  (declaration (annotation-property !obo:PROXXX_201))
	  (annotation-assertion !rdfs:label !obo:PROXXX_201 "residue letter code")
	  (sub-annotation-property-of !obo:PROXXX_201 !'alternative term'@iao)
	  (declaration (annotation-property !obo:PROXXX_202))
	  (annotation-assertion !rdfs:label !obo:PROXXX_202 "RESID accession")
	  (sub-annotation-property-of !obo:PROXXX_202 !'alternative term'@iao)
	  (declaration (class !obo:PROXXX_203))
	  (annotation-assertion !rdfs:label !obo:PROXXX_203 "cross-linked residues")
	  (sub-class-of !obo:PROXXX_203 !obo:CHEBI_33247)
	  (declaration (data-property !obo:PROXXX_204))
	  (annotation-assertion !rdfs:label !obo:PROXXX_204 "has ordinal position")
	  (declaration (data-property !obo:PROXXX_205))
	  (annotation-assertion !rdfs:label !obo:PROXXX_205 "within uniprot reference sequence")
	  (annotation-assertion !'curator note'@iao !obo:PROXXX_205 "2014-08-01 Alan Ruttenberg: Ugh - FIXME")
	  (declaration (class !obo:_obsolete))
	  (declaration (class !obo:CHEBI_33708)) ; amino acid residue
	  )
	 (each-resid-entry
	  (read-resid)
	  (lambda (&key entry id iri label synonyms 1-letter-code 3-letter-code mod-accession definition-sources formula cross-linked deprecated)
	    (push iri all-resids)
	    (as `(declaration (class ,iri))
		`(annotation-assertion !rdfs:label ,iri ,label)
		`(annotation-assertion !'editor preferred term'@iao ,iri ,label)
		`(annotation-assertion !obo:PROXXX_202 ,iri ,id))
	    (and formula (as `(annotation-assertion 
			       !'definition'@iao ,iri 
			       ,(if cross-linked
				    (format nil "An amino acid residue with the formula ~a" formula)
				    (format nil "~a cross-linked amino acid residues with the formula ~a" cross-linked formula)))))

	    (if deprecated
		(progn
		  (as `(sub-class-of ,iri !obo:_obsolete)
		      `(annotation-assertion !owl:deprecated ,iri !xsd:true)))
		(if (plusp cross-linked)
		    (as `(sub-class-of ,iri !obo:PROXXX_203))
		    (as `(sub-class-of ,iri !obo:CHEBI_33708))))
	    (mapcar (lambda(cite) (as `(annotation-assertion !'definition source'@iao ,iri ,cite))) definition-sources)
	    (mapcar (lambda(syn) (as `(annotation-assertion !'alternative term'@iao ,iri ,syn))) synonyms)
	    (as `(annotation-assertion !'term editor'@iao ,iri "John S. Garavelli"))
	    (loop for mod-a in mod-accession do (as `(annotation-assertion !obo:PROXXX_200 ,iri ,mod-a)))
	    (and 1-letter-code (as `(annotation-assertion !obo:PROXXX_201 ,iri ,1-letter-code)))))
	 (as `(disjoint-classes ,@all-resids)))
      (write-rdfxml resid))))
	

;; '("Entry" (("id" "AA0001"))
;;  ("Header" NIL ("Code" NIL "AA0001")
;;   ("Dates" NIL ("CreationDate" NIL "31-Mar-1995")
;;    ("StrucRevDate" NIL "31-Mar-1995")
;;    ("TextChngDate" NIL "30-Sep-2010")))
;;  ("Names" NIL ("Name" NIL "L-alanine")
;;   ("AlternateName" NIL "2-aminopropionic acid")
;;   ("AlternateName" NIL "2-azanylpropanoic acid")
;;   ("AlternateName" NIL "alpha-alanine")
;;   ("AlternateName" NIL "alpha-aminopropionic acid")
;;   ("SystematicName" NIL "(2S)-2-aminopropanoic acid")
;;   ("Xref" NIL "CAS:56-41-7") ("Xref" NIL "ChEBI:46217")
;;   ("Xref" NIL "PDBHET:ALA"))
;;  ("FormulaBlock" NIL ("Formula" NIL "C 3 H 5 N 1 O 1")
;;   ("Weight" (("type" "chemical")) "71.08")
;;   ("Weight" (("type" "physical")) "71.037114"))
;;  ("CorrectionBlock" (("label" "ALA") ("uids" "AA0001"))
;;   ("Formula" NIL "C 0 H 0 N 0 O 0")
;;   ("Weight" (("type" "chemical")) "0.00")
;;   ("Weight" (("type" "physical")) "0.000000"))
;;  ("CorrectionBlock" (("label" "ASP") ("uids" "AA0004"))
;;   ("Formula" NIL "C -1 H 0 N 0 O -2")
;;   ("Weight" (("type" "chemical")) "-44.01")
;;   ("Weight" (("type" "physical")) "-43.989829"))
;;  ("ReferenceBlock" NIL
;;   ("Authors" NIL ("Anonymous" NIL)
;;    ("Group" NIL
;;     "IUPAC-IUB Joint Commission on Biochemical Nomenclature (JCBN)"))
;;   ("Citation" NIL "Eur. J. Biochem. 138, 9-37, 1984")
;;   ("Title" NIL
;;    "Nomenclature and symbolism for amino acids and peptides. Recommendations 1983.")
;;   ("Xref" NIL "DOI:10.1111/j.1432-1033.1984.tb07877.x")
;;   ("Xref" NIL "PMID:6692818")
;;   ("Note" NIL
;;    "standard three-letter and one-letter symbols, and nomenclature"))
;;  ("ReferenceBlock" NIL
;;   ("Authors" NIL ("Author" NIL "Jankowski, V.")
;;    ("Author" NIL "Vanholder, R.") ("Author" NIL "van der Giet, M.")
;;    ("Author" NIL "Tölle, M.") ("Author" NIL "Karadogan, S.")
;;    ("Author" NIL "Gobom, J.") ("Author" NIL "Furkert, J.")
;;    ("Author" NIL "Oksche, A.") ("Author" NIL "Krause, E.")
;;    ("Author" NIL "Tran, T.N.") ("Author" NIL "Tepel, M.")
;;    ("Author" NIL "Schuchardt, M.") ("Author" NIL "Schlüter, H.")
;;    ("Author" NIL "Wiedon, A.") ("Author" NIL "Beyermann, M.")
;;    ("Author" NIL "Bader, M.") ("Author" NIL "Todiras, M.")
;;    ("Author" NIL "Zidek, W.") ("Author" NIL "Jankowski, J."))
;;   ("Citation" NIL
;;    "Arterioscler. Thromb. Vasc. Biol. 27, 297-302, 2007")
;;   ("Title" NIL
;;    "Mass-spectrometric identification of a novel angiotensin peptide in human plasma.")
;;   ("Xref" NIL "DOI:10.1161/01.ATV.0000253889.09765.5f")
;;   ("Xref" NIL "PMID:17138938")
;;   ("Note" NIL
;;    "identification of an alanine residue apparently arising from a decarboxylated aspartic acid residue"))
;;  ("Comment" NIL
;;   "Alanine has not been reported to act as an active site residue.")
;;  ("SequenceCode" (("link" "ALA")) ("SequenceSpec" NIL "A")
;;   ("Abbreviation" NIL "Ala") ("Xref" NIL "PSI-MOD:00010"))
;;  ("SequenceCode" (("link" "ASP")) ("SequenceSpec" NIL "D")
;;   ("Xref" NIL "PSI-MOD:00869"))
;;  ("Source" NIL "natural")
;;  ("Features" NIL
;;   ("FeatureNote" NIL
;;    ("Feature" (("link" "ALA") ("type" "UniProt")) "Not available")
;;    ("Note" NIL
;;     "UniProt has no active site feature annotations for this residue"))
;;   ("Feature"
;;    (("link" "ASP")
;;     ("key" "mod_res__beta-decarboxylated aspartate__d")
;;     ("type" "UniProt"))
;;    "MOD_RES Beta-decarboxylated aspartate"))
;;  ("Image" (("alt" "DUMMY.GIF") ("src" "/images/AA0001.GIF")))
;;  ("Model" (("src" "/models/AA0001.PDB"))))

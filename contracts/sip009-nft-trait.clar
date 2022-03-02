;; sip009 must conform to specific implementation
;; Clarity does not follow a class hierarchy model, instead it uses traits
;; which define public interface of contract
(define-trait sip009-nft-trait
	(
		;; Last token ID, limited to uint range
		(get-last-token-id () (response uint uint))

		;; URI for metadata associated with the token 
		(get-token-uri (uint) (response (optional (string-ascii 256)) uint))

		;; Owner of a given token identifier
		(get-owner (uint) (response (optional principal) uint))

		;; Transfer from the sender to a new principal
		(transfer (uint principal principal) (response bool uint))
	)
)
;; parameters are not explicitly defined by name because only the type is important
;; the defined parameters can be found in sip documentation
;; eg. transfer: uint = token id, principal = sender, principal = recipient
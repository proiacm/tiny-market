;; import sip traits
(use-trait nft-trait .sip009-nft-trait.sip009-nft-trait)
(use-trait ft-trait .sip010-ft-trait.sip010-ft-trait)

;; define contract owner
(define-constant contract-owner tx-sender)

;; Think about the various error states that exist in our marketplace.
;; Listing an NFT may fail under a number of circumstances; namely,
;; the expiry block height is in the past, or the listing price is zero (we will not allow free listings).
;; There is also the consideration of the using trying to list the NFT not actually owning it,
;; but this will be handled by the NFT contract itself.
;; listing errors
(define-constant err-expiry-in-past (err u1000))
(define-constant err-price-zero (err u1001))

;; When it comes to cancelling and fulfilling, there are a few more error conditions we can identify:
;; The listing the tx-sender wants to cancel or fulfil does not exist.
;; The tx-sender tries to cancel a listing it did not create.
;; The listing the tx-sender tries to fill has expired.
;; The provided NFT asset trait reference does not match the NFT contract of the listing.
;; Since trait references cannot be stored directly in Clarity, they will have to be provided again when
;; the buyer is trying to purchase an NFT. We have to make sure that the trait reference provided by the buyer
;; matches the NFT contract provided by the seller.
;; The provided payment asset trait reference does not match the payment asset contract of the listing.
;; The same as the above but for the SIP010 being used to purchase the NFT.
;; The maker and the taker (seller and the buyer) are equal. We will not permit users to purchase tokens
;; from themselves using the same principal.
;; The buyer is not the intended taker. If the seller defines an intended taker (buyer) for the listing,
;; then only that principal can fulfil the listing.
;; cancelling and fulfilling errors
(define-constant err-unknown-listing (err u2000))
(define-constant err-unauthorised (err u2001))
(define-constant err-listing-expired (err u2002))
(define-constant err-nft-asset-mismatch (err u2003))
(define-constant err-payment-asset-mismatch (err u2004))
(define-constant err-maker-taker-equal (err u2005))
(define-constant err-unintended-taker (err u2006))

;; Finally, we will implement a whitelist for NFT and payment asset contracts that the contract deployer controls.
;; It makes for two additional error conditions:
;; The NFT asset the seller is trying to list is not whitelisted.
;; The requested payment asset is not whitelisted.
(define-constant err-asset-contract-not-whitelisted (err u2007))
(define-constant err-payment-contract-not-whitelisted (err u2008))

;; The most efficient way to store the individual listings is by using a data map that uses
;; an unsigned integer as a key. The integer functions as a unique identifier and will increment
;; for each new listing. We will never reuse a value. To track the latest listing ID, we will use a simple data variable.
(define-map listings
	uint
	{
		maker: principal,
		taker: (optional principal),
		token-id: uint,
		nft-asset-contract: principal,
		expiry: uint,
		price: uint,
    ;; It is important to utilise the native types in Clarity to the fullest extent possible.
    ;; A listing does not need to have an intended taker, so we make it optional.
		payment-asset-contract: (optional principal)
	}
)

(define-data-var listing-nonce uint u0)

;; The whitelist itself is a simple map that stores a boolean for a given contract principal.
;; A guarded public function set-whitelisted is used to update the whitelist and a read-only function
;; is-whitelisted allows anyone to check if a particular contract is whitelisted or not.
;; We will also use is-whitelisted to guard other public functions later.
(define-map whitelisted-asset-contracts principal bool)

(define-read-only (is-whitelisted (asset-contract principal))
	(default-to false (map-get? whitelisted-asset-contracts asset-contract))
)

(define-public (set-whitelisted (asset-contract principal) (whitelisted bool))
	(begin
		(asserts! (is-eq contract-owner tx-sender) err-unauthorised)
		(ok (map-set whitelisted-asset-contracts asset-contract whitelisted))
	)
)
;; take a trait reference (either SIP009 or SIP010) and then do the proper contract-call? to transfer the token.
(define-private (transfer-nft (token-contract <nft-trait>) (token-id uint) (sender principal) (recipient principal))
	(contract-call? token-contract transfer token-id sender recipient)
)

(define-private (transfer-ft (token-contract <ft-trait>) (amount uint) (sender principal) (recipient principal))
	(contract-call? token-contract transfer amount sender recipient none)
)

;; Retrieve the current listing ID to use by reading the listing-nonce variable.
;; Assert that the NFT asset is whitelisted.
;; Assert that the provided expiry height is somewhere in the future.
;; Assert that the listing price is larger than zero.
;; If a payment asset is given, assert that it is whitelisted.
;; Transfer the NFT from the tx-sender to the marketplace.
;; Store the listing information in the listings data map.
;; Increment the listing-nonce variable.
;; Return an ok to materialise the changes.
(define-public (list-asset (nft-asset-contract <nft-trait>) (nft-asset {taker: (optional principal), token-id: uint, expiry: uint, price: uint, payment-asset-contract: (optional principal)}))
	(let ((listing-id (var-get listing-nonce)))
		(asserts! (is-whitelisted (contract-of nft-asset-contract)) err-asset-contract-not-whitelisted)
		(asserts! (> (get expiry nft-asset) block-height) err-expiry-in-past)
		(asserts! (> (get price nft-asset) u0) err-price-zero)
		(asserts! (match (get payment-asset-contract nft-asset) payment-asset (is-whitelisted payment-asset) true) err-payment-contract-not-whitelisted)
		(try! (transfer-nft nft-asset-contract (get token-id nft-asset) tx-sender (as-contract tx-sender)))
		(map-set listings listing-id (merge {maker: tx-sender, nft-asset-contract: (contract-of nft-asset-contract)} nft-asset))
		(var-set listing-nonce (+ listing-id u1))
		(ok listing-id)
	)
)

(define-read-only (get-listing (listing-id uint))
	(map-get? listings listing-id)
)

;; A listing is available until it either expires or is cancelled by the maker.
;; When the maker cancels the listing, all that has to happen is for the marketplace to send
;; the NFT back and delete the listing from the data map. The maker only has to provide the
;; listing ID and the NFT asset contract trait reference. The rest can be read from the data map.
(define-public (cancel-listing (listing-id uint) (nft-asset-contract <nft-trait>))
	(let (
		(listing (unwrap! (map-get? listings listing-id) err-unknown-listing))
		(maker (get maker listing))
		)
		(asserts! (is-eq maker tx-sender) err-unauthorised)
		(asserts! (is-eq (get nft-asset-contract listing) (contract-of nft-asset-contract)) err-nft-asset-mismatch)
		(map-delete listings listing-id)
		(as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender maker))
	)
)

;; Retrieve the listing from the listings data map and abort if it does not exist.
;; Assert that the taker is not equal to the maker.
;; Assert that the expiry block height has not been reached.
;; Assert that the provided NFT trait reference is equal to the principal stored in the listing.
;; Assert that the payment asset trait reference, if any, is equal to the one stored in the listing.
;; Transfer the NFT from the contract to the buyer and the payment asset from the buyer to the seller and revert if either transfer fails.
;; Delete the listing from the listings data map.
(define-private (assert-can-fulfil (nft-asset-contract principal) (payment-asset-contract (optional principal)) (listing {maker: principal, taker: (optional principal), token-id: uint, nft-asset-contract: principal, expiry: uint, price: uint, payment-asset-contract: (optional principal)}))
	(begin
		(asserts! (not (is-eq (get maker listing) tx-sender)) err-maker-taker-equal)
		(asserts! (match (get taker listing) intended-taker (is-eq intended-taker tx-sender) true) err-unintended-taker)
		(asserts! (< block-height (get expiry listing)) err-listing-expired)
		(asserts! (is-eq (get nft-asset-contract listing) nft-asset-contract) err-nft-asset-mismatch)
		(asserts! (is-eq (get payment-asset-contract listing) payment-asset-contract) err-payment-asset-mismatch)
		(ok true)
	)
)

(define-public (fulfil-listing-stx (listing-id uint) (nft-asset-contract <nft-trait>))
	(let (
		(listing (unwrap! (map-get? listings listing-id) err-unknown-listing))
		(taker tx-sender)
		)
		(try! (assert-can-fulfil (contract-of nft-asset-contract) none listing))
		(try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender taker)))
		(try! (stx-transfer? (get price listing) taker (get maker listing)))
		(map-delete listings listing-id)
		(ok listing-id)
	)
)

(define-public (fulfil-listing-ft (listing-id uint) (nft-asset-contract <nft-trait>) (payment-asset-contract <ft-trait>))
	(let (
		(listing (unwrap! (map-get? listings listing-id) err-unknown-listing))
		(taker tx-sender)
		)
		(try! (assert-can-fulfil (contract-of nft-asset-contract) (some (contract-of payment-asset-contract)) listing))
		(try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender taker)))
		(try! (transfer-ft payment-asset-contract (get price listing) taker (get maker listing)))
		(map-delete listings listing-id)
		(ok listing-id)
	)
)
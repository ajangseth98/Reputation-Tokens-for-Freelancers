(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-cooldown-active (err u103))
(define-constant err-invalid-rating (err u104))
(define-constant err-already-rated (err u105))

(define-non-fungible-token reputation-token uint)

(define-map freelancer-profiles
    principal
    {
        total-score: uint,
        reviews-count: uint,
        tier-level: uint,
        last-review-height: uint
    }
)

(define-map gig-records
    uint
    {
        freelancer: principal,
        client: principal,
        completed: bool,
        rating: uint,
        stake-amount: uint,
        description: (string-ascii 256)
    }
)

(define-map client-reviews
    {gig-id: uint, client: principal}
    bool
)

(define-data-var last-token-id uint u0)
(define-data-var cooldown-period uint u144) ;; ~24 hours in blocks
(define-data-var minimum-stake uint u1000000) ;; in microSTX

(define-public (initialize-freelancer)
    (let
        ((sender tx-sender))
        (asserts! (is-none (map-get? freelancer-profiles sender)) (err u106))
        (ok (map-set freelancer-profiles
            sender
            {
                total-score: u0,
                reviews-count: u0,
                tier-level: u1,
                last-review-height: u0
            }
        ))
    )
)

(define-public (create-gig (description (string-ascii 256)))
    (let
        ((new-id (+ (var-get last-token-id) u1))
         (stake-requirement (var-get minimum-stake)))
        (try! (stx-transfer? stake-requirement tx-sender (as-contract tx-sender)))
        (map-set gig-records new-id
            {
                freelancer: tx-sender,
                client: tx-sender,
                completed: false,
                rating: u0,
                stake-amount: stake-requirement,
                description: description
            }
        )
        (var-set last-token-id new-id)
        (mint-token new-id tx-sender)
    )
)

(define-public (complete-gig (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found)))
        (asserts! (is-eq (get freelancer gig) tx-sender) err-unauthorized)
        (asserts! (not (get completed gig)) err-already-rated)
        (ok (map-set gig-records gig-id
            (merge gig {completed: true})))
    )
)

(define-public (submit-rating (gig-id uint) (rating uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (current-height burn-block-height)
         (freelancer-data (unwrap! (map-get? freelancer-profiles (get freelancer gig)) err-not-found)))
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (get completed gig) err-unauthorized)
        (asserts! (is-none (map-get? client-reviews {gig-id: gig-id, client: tx-sender})) err-already-rated)
        
        (try! (update-freelancer-profile (get freelancer gig) rating))
        (map-set client-reviews {gig-id: gig-id, client: tx-sender} true)
        (ok true)
    )
)

(define-private (update-freelancer-profile (freelancer principal) (rating uint))
    (let
        ((profile (unwrap! (map-get? freelancer-profiles freelancer) err-not-found))
         (new-total (+ (get total-score profile) rating))
         (new-count (+ (get reviews-count profile) u1))
         (new-tier (calculate-tier new-total new-count)))
        (ok (map-set freelancer-profiles
            freelancer
            {
                total-score: new-total,
                reviews-count: new-count,
                tier-level: new-tier,
                last-review-height: burn-block-height
            }
        ))
    )
)

(define-private (calculate-tier (total uint) (count uint))
    (let
        ((average (/ total count)))
        (if (>= average u4)
            u3
            (if (>= average u3)
                u2
                u1
            )
        )
    )
)

(define-read-only (get-freelancer-profile (freelancer principal))
    (map-get? freelancer-profiles freelancer)
)

(define-read-only (get-gig-details (gig-id uint))
    (map-get? gig-records gig-id)
)

(define-read-only (get-average-rating (freelancer principal))
    (match (map-get? freelancer-profiles freelancer)
        profile (ok (/ (get total-score profile) (get reviews-count profile)))
        err-not-found
    )
)

(define-private (mint-token (id uint) (recipient principal))
    (nft-mint? reputation-token id recipient)
)

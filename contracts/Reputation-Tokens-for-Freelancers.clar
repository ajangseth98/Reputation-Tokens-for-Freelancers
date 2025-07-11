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

(define-constant err-dispute-exists (err u107))
(define-constant err-dispute-not-found (err u108))
(define-constant err-dispute-resolved (err u109))
(define-constant err-invalid-dispute-status (err u110))

(define-map gig-disputes
    uint
    {
        gig-id: uint,
        client: principal,
        freelancer: principal,
        reason: (string-ascii 256),
        status: uint,
        resolution: (string-ascii 256),
        created-height: uint,
        resolved-height: uint
    }
)

(define-data-var dispute-counter uint u0)

(define-public (create-dispute (gig-id uint) (reason (string-ascii 256)))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (dispute-id (+ (var-get dispute-counter) u1)))
        (asserts! (get completed gig) err-unauthorized)
        (asserts! (is-none (map-get? gig-disputes gig-id)) err-dispute-exists)
        (map-set gig-disputes gig-id
            {
                gig-id: gig-id,
                client: tx-sender,
                freelancer: (get freelancer gig),
                reason: reason,
                status: u1,
                resolution: "",
                created-height: burn-block-height,
                resolved-height: u0
            }
        )
        (var-set dispute-counter dispute-id)
        (ok dispute-id)
    )
)

(define-public (resolve-dispute (gig-id uint) (resolution (string-ascii 256)) (favor-freelancer bool))
    (let
        ((dispute (unwrap! (map-get? gig-disputes gig-id) err-dispute-not-found))
         (gig (unwrap! (map-get? gig-records gig-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (get status dispute) u1) err-dispute-resolved)
        (if favor-freelancer
            (try! (stx-transfer? (get stake-amount gig) (as-contract tx-sender) (get freelancer gig)))
            (try! (stx-transfer? (get stake-amount gig) (as-contract tx-sender) (get client dispute)))
        )
        (map-set gig-disputes gig-id
            (merge dispute {
                status: u2,
                resolution: resolution,
                resolved-height: burn-block-height
            })
        )
        (ok true)
    )
)

(define-read-only (get-dispute-details (gig-id uint))
    (map-get? gig-disputes gig-id)
)

(define-constant err-certification-exists (err u111))
(define-constant err-certification-not-found (err u112))
(define-constant err-insufficient-tier (err u113))
(define-constant err-certification-fee (err u114))

(define-map skill-certifications
    {freelancer: principal, skill: (string-ascii 64)}
    {
        certified: bool,
        certification-date: uint,
        expiry-date: uint,
        fee-paid: uint
    }
)

(define-map certification-requirements
    (string-ascii 64)
    {
        min-tier: uint,
        min-reviews: uint,
        certification-fee: uint,
        validity-period: uint
    }
)

(define-data-var certification-fee-base uint u500000)

(define-public (setup-certification (skill (string-ascii 64)) (min-tier uint) (min-reviews uint) (fee uint) (validity uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set certification-requirements skill
            {
                min-tier: min-tier,
                min-reviews: min-reviews,
                certification-fee: fee,
                validity-period: validity
            }
        ))
    )
)

(define-public (apply-for-certification (skill (string-ascii 64)))
    (let
        ((freelancer-data (unwrap! (map-get? freelancer-profiles tx-sender) err-not-found))
         (cert-req (unwrap! (map-get? certification-requirements skill) err-certification-not-found))
         (existing-cert (map-get? skill-certifications {freelancer: tx-sender, skill: skill})))
        (asserts! (>= (get tier-level freelancer-data) (get min-tier cert-req)) err-insufficient-tier)
        (asserts! (>= (get reviews-count freelancer-data) (get min-reviews cert-req)) err-insufficient-tier)
        (asserts! (is-none existing-cert) err-certification-exists)
        (try! (stx-transfer? (get certification-fee cert-req) tx-sender contract-owner))
        (map-set skill-certifications {freelancer: tx-sender, skill: skill}
            {
                certified: true,
                certification-date: burn-block-height,
                expiry-date: (+ burn-block-height (get validity-period cert-req)),
                fee-paid: (get certification-fee cert-req)
            }
        )
        (ok true)
    )
)

(define-public (renew-certification (skill (string-ascii 64)))
    (let
        ((cert (unwrap! (map-get? skill-certifications {freelancer: tx-sender, skill: skill}) err-certification-not-found))
         (cert-req (unwrap! (map-get? certification-requirements skill) err-certification-not-found)))
        (asserts! (get certified cert) err-certification-not-found)
        (try! (stx-transfer? (get certification-fee cert-req) tx-sender contract-owner))
        (map-set skill-certifications {freelancer: tx-sender, skill: skill}
            (merge cert {
                certification-date: burn-block-height,
                expiry-date: (+ burn-block-height (get validity-period cert-req)),
                fee-paid: (get certification-fee cert-req)
            })
        )
        (ok true)
    )
)

(define-read-only (get-certification-status (freelancer principal) (skill (string-ascii 64)))
    (match (map-get? skill-certifications {freelancer: freelancer, skill: skill})
        cert (ok {
            certified: (and (get certified cert) (> (get expiry-date cert) burn-block-height)),
            expiry-date: (get expiry-date cert)
        })
        (ok {certified: false, expiry-date: u0})
    )
)

(define-read-only (get-certification-requirements (skill (string-ascii 64)))
    (map-get? certification-requirements skill)
)
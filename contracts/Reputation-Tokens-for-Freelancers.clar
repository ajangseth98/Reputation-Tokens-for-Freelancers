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
(define-constant err-escrow-exists (err u115))
(define-constant err-escrow-not-found (err u116))
(define-constant err-insufficient-balance (err u117))
(define-constant err-payment-released (err u118))

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

(define-map escrow-payments
    uint
    {
        gig-id: uint,
        client: principal,
        freelancer: principal,
        amount: uint,
        created-height: uint,
        released: bool,
        penalty-applied: bool
    }
)

(define-data-var penalty-rate uint u10)

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

(define-public (deposit-escrow (gig-id uint) (amount uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (existing-escrow (map-get? escrow-payments gig-id)))
        (asserts! (is-eq tx-sender (get client gig)) err-unauthorized)
        (asserts! (not (get completed gig)) err-unauthorized)
        (asserts! (is-none existing-escrow) err-escrow-exists)
        (asserts! (> amount u0) err-insufficient-balance)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set escrow-payments gig-id
            {
                gig-id: gig-id,
                client: tx-sender,
                freelancer: (get freelancer gig),
                amount: amount,
                created-height: burn-block-height,
                released: false,
                penalty-applied: false
            }
        )
        (ok true)
    )
)

(define-public (release-escrow-payment (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (escrow (unwrap! (map-get? escrow-payments gig-id) err-escrow-not-found)))
        (asserts! (get completed gig) err-unauthorized)
        (asserts! (not (get released escrow)) err-payment-released)
        (asserts! (>= (get rating gig) u3) err-invalid-rating)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get freelancer escrow))))
        (map-set escrow-payments gig-id
            (merge escrow {released: true})
        )
        (ok true)
    )
)

(define-public (emergency-withdraw-escrow (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (escrow (unwrap! (map-get? escrow-payments gig-id) err-escrow-not-found))
         (penalty-amount (/ (* (get amount escrow) (var-get penalty-rate)) u100))
         (refund-amount (- (get amount escrow) penalty-amount)))
        (asserts! (is-eq tx-sender (get client escrow)) err-unauthorized)
        (asserts! (not (get released escrow)) err-payment-released)
        (asserts! (> (- burn-block-height (get created-height escrow)) u1008) err-cooldown-active)
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get client escrow))))
        (try! (as-contract (stx-transfer? penalty-amount tx-sender contract-owner)))
        (map-set escrow-payments gig-id
            (merge escrow {released: true, penalty-applied: true})
        )
        (ok refund-amount)
    )
)

(define-read-only (get-escrow-details (gig-id uint))
    (map-get? escrow-payments gig-id)
)

(define-read-only (get-escrow-balance (gig-id uint))
    (match (map-get? escrow-payments gig-id)
        escrow (if (get released escrow) (ok u0) (ok (get amount escrow)))
        err-escrow-not-found
    )
)

(define-constant err-portfolio-exists (err u119))
(define-constant err-portfolio-not-found (err u120))
(define-constant err-portfolio-item-exists (err u121))
(define-constant err-portfolio-item-not-found (err u122))
(define-constant err-invalid-portfolio-data (err u123))
(define-constant err-portfolio-full (err u124))

(define-map freelancer-portfolios
    principal
    {
        bio: (string-ascii 512),
        experience-years: uint,
        hourly-rate: uint,
        availability: bool,
        languages: (list 10 (string-ascii 32)),
        created-height: uint,
        updated-height: uint,
        total-items: uint
    }
)

(define-map portfolio-items
    {freelancer: principal, item-id: uint}
    {
        title: (string-ascii 128),
        description: (string-ascii 512),
        category: (string-ascii 64),
        completion-date: uint,
        client-feedback: (string-ascii 256),
        project-value: uint,
        skills-used: (list 10 (string-ascii 32)),
        status: uint
    }
)

(define-data-var max-portfolio-items uint u20)

(define-public (create-portfolio (bio (string-ascii 512)) (experience-years uint) (hourly-rate uint) (languages (list 10 (string-ascii 32))))
    (let
        ((freelancer tx-sender))
        (asserts! (is-some (map-get? freelancer-profiles freelancer)) err-not-found)
        (asserts! (is-none (map-get? freelancer-portfolios freelancer)) err-portfolio-exists)
        (asserts! (and (> (len bio) u0) (<= experience-years u50) (> hourly-rate u0)) err-invalid-portfolio-data)
        (map-set freelancer-portfolios freelancer
            {
                bio: bio,
                experience-years: experience-years,
                hourly-rate: hourly-rate,
                availability: true,
                languages: languages,
                created-height: burn-block-height,
                updated-height: burn-block-height,
                total-items: u0
            }
        )
        (ok true)
    )
)

(define-public (update-portfolio-info (bio (string-ascii 512)) (hourly-rate uint) (availability bool) (languages (list 10 (string-ascii 32))))
    (let
        ((portfolio (unwrap! (map-get? freelancer-portfolios tx-sender) err-portfolio-not-found)))
        (asserts! (and (> (len bio) u0) (> hourly-rate u0)) err-invalid-portfolio-data)
        (map-set freelancer-portfolios tx-sender
            (merge portfolio {
                bio: bio,
                hourly-rate: hourly-rate,
                availability: availability,
                languages: languages,
                updated-height: burn-block-height
            })
        )
        (ok true)
    )
)

(define-public (add-portfolio-item (title (string-ascii 128)) (description (string-ascii 512)) (category (string-ascii 64)) (completion-date uint) (client-feedback (string-ascii 256)) (project-value uint) (skills-used (list 10 (string-ascii 32))))
    (let
        ((portfolio (unwrap! (map-get? freelancer-portfolios tx-sender) err-portfolio-not-found))
         (new-item-id (+ (get total-items portfolio) u1)))
        (asserts! (< (get total-items portfolio) (var-get max-portfolio-items)) err-portfolio-full)
        (asserts! (and (> (len title) u0) (> (len description) u0) (> (len category) u0)) err-invalid-portfolio-data)
        (asserts! (is-none (map-get? portfolio-items {freelancer: tx-sender, item-id: new-item-id})) err-portfolio-item-exists)
        (map-set portfolio-items {freelancer: tx-sender, item-id: new-item-id}
            {
                title: title,
                description: description,
                category: category,
                completion-date: completion-date,
                client-feedback: client-feedback,
                project-value: project-value,
                skills-used: skills-used,
                status: u1
            }
        )
        (map-set freelancer-portfolios tx-sender
            (merge portfolio {
                total-items: new-item-id,
                updated-height: burn-block-height
            })
        )
        (ok new-item-id)
    )
)

(define-public (update-portfolio-item (item-id uint) (title (string-ascii 128)) (description (string-ascii 512)) (client-feedback (string-ascii 256)) (status uint))
    (let
        ((item (unwrap! (map-get? portfolio-items {freelancer: tx-sender, item-id: item-id}) err-portfolio-item-not-found)))
        (asserts! (and (> (len title) u0) (> (len description) u0) (<= status u2)) err-invalid-portfolio-data)
        (map-set portfolio-items {freelancer: tx-sender, item-id: item-id}
            (merge item {
                title: title,
                description: description,
                client-feedback: client-feedback,
                status: status
            })
        )
        (ok true)
    )
)

(define-public (toggle-availability)
    (let
        ((portfolio (unwrap! (map-get? freelancer-portfolios tx-sender) err-portfolio-not-found)))
        (map-set freelancer-portfolios tx-sender
            (merge portfolio {
                availability: (not (get availability portfolio)),
                updated-height: burn-block-height
            })
        )
        (ok (not (get availability portfolio)))
    )
)

(define-read-only (get-portfolio (freelancer principal))
    (map-get? freelancer-portfolios freelancer)
)

(define-read-only (get-portfolio-item (freelancer principal) (item-id uint))
    (map-get? portfolio-items {freelancer: freelancer, item-id: item-id})
)

(define-read-only (get-freelancer-summary (freelancer principal))
    (match (map-get? freelancer-profiles freelancer)
        profile (match (map-get? freelancer-portfolios freelancer)
            portfolio (ok {
                reputation: (get tier-level profile),
                total-reviews: (get reviews-count profile),
                average-rating: (if (> (get reviews-count profile) u0) 
                    (/ (get total-score profile) (get reviews-count profile)) u0),
                experience-years: (get experience-years portfolio),
                hourly-rate: (get hourly-rate portfolio),
                availability: (get availability portfolio),
                total-portfolio-items: (get total-items portfolio)
            })
            (ok {
                reputation: (get tier-level profile),
                total-reviews: (get reviews-count profile),
                average-rating: (if (> (get reviews-count profile) u0) 
                    (/ (get total-score profile) (get reviews-count profile)) u0),
                experience-years: u0,
                hourly-rate: u0,
                availability: false,
                total-portfolio-items: u0
            })
        )
        err-not-found
    )
)

(define-read-only (is-freelancer-available (freelancer principal))
    (match (map-get? freelancer-portfolios freelancer)
        portfolio (ok (get availability portfolio))
        (ok false)
    )
)

;; ============================================
;; MILESTONE TRACKER FEATURE
;; ============================================

(define-constant err-milestone-not-found (err u125))
(define-constant err-milestone-already-approved (err u126))
(define-constant err-milestone-not-approved (err u127))
(define-constant err-milestone-already-completed (err u128))
(define-constant err-invalid-milestone-data (err u129))
(define-constant err-insufficient-milestone-payment (err u130))
(define-constant err-milestone-payment-released (err u131))
(define-constant err-too-many-milestones (err u132))

(define-map gig-milestones
    {gig-id: uint, milestone-id: uint}
    {
        title: (string-ascii 128),
        description: (string-ascii 256),
        payment-amount: uint,
        deadline-height: uint,
        status: uint, ;; 1=pending, 2=approved, 3=completed, 4=paid
        created-height: uint,
        approved-height: uint,
        completed-height: uint,
        paid-height: uint
    }
)

(define-map gig-milestone-counters
    uint ;; gig-id
    {
        total-milestones: uint,
        completed-milestones: uint,
        total-milestone-value: uint,
        paid-milestone-value: uint
    }
)

(define-map milestone-payments
    {gig-id: uint, milestone-id: uint}
    {
        client: principal,
        freelancer: principal,
        amount: uint,
        escrowed: bool,
        released: bool,
        created-height: uint
    }
)

(define-data-var max-milestones-per-gig uint u10)

(define-public (create-milestone (gig-id uint) (title (string-ascii 128)) (description (string-ascii 256)) (payment-amount uint) (deadline-blocks uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (milestone-counter (default-to {total-milestones: u0, completed-milestones: u0, total-milestone-value: u0, paid-milestone-value: u0} 
                           (map-get? gig-milestone-counters gig-id)))
         (new-milestone-id (+ (get total-milestones milestone-counter) u1))
         (deadline-height (+ burn-block-height deadline-blocks)))
        (asserts! (is-eq tx-sender (get freelancer gig)) err-unauthorized)
        (asserts! (not (get completed gig)) err-unauthorized)
        (asserts! (< (get total-milestones milestone-counter) (var-get max-milestones-per-gig)) err-too-many-milestones)
        (asserts! (and (> (len title) u0) (> (len description) u0) (> payment-amount u0) (> deadline-blocks u0)) err-invalid-milestone-data)
        
        ;; Create milestone
        (map-set gig-milestones {gig-id: gig-id, milestone-id: new-milestone-id}
            {
                title: title,
                description: description,
                payment-amount: payment-amount,
                deadline-height: deadline-height,
                status: u1, ;; pending
                created-height: burn-block-height,
                approved-height: u0,
                completed-height: u0,
                paid-height: u0
            }
        )
        
        ;; Update counter
        (map-set gig-milestone-counters gig-id
            (merge milestone-counter {
                total-milestones: new-milestone-id,
                total-milestone-value: (+ (get total-milestone-value milestone-counter) payment-amount)
            })
        )
        
        (ok new-milestone-id)
    )
)

(define-public (approve-milestone (gig-id uint) (milestone-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (milestone (unwrap! (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id}) err-milestone-not-found)))
        (asserts! (is-eq tx-sender (get client gig)) err-unauthorized)
        (asserts! (is-eq (get status milestone) u1) err-milestone-already-approved)
        
        ;; Update milestone status
        (map-set gig-milestones {gig-id: gig-id, milestone-id: milestone-id}
            (merge milestone {
                status: u2, ;; approved
                approved-height: burn-block-height
            })
        )
        
        (ok true)
    )
)

(define-public (complete-milestone (gig-id uint) (milestone-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (milestone (unwrap! (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id}) err-milestone-not-found)))
        (asserts! (is-eq tx-sender (get freelancer gig)) err-unauthorized)
        (asserts! (is-eq (get status milestone) u2) err-milestone-not-approved)
        (asserts! (not (is-eq (get status milestone) u3)) err-milestone-already-completed)
        
        ;; Update milestone status
        (map-set gig-milestones {gig-id: gig-id, milestone-id: milestone-id}
            (merge milestone {
                status: u3, ;; completed
                completed-height: burn-block-height
            })
        )
        
        ;; Update gig milestone counter
        (let
            ((counter (unwrap! (map-get? gig-milestone-counters gig-id) err-not-found)))
            (map-set gig-milestone-counters gig-id
                (merge counter {
                    completed-milestones: (+ (get completed-milestones counter) u1)
                })
            )
        )
        
        (ok true)
    )
)

(define-public (escrow-milestone-payment (gig-id uint) (milestone-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (milestone (unwrap! (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id}) err-milestone-not-found))
         (existing-payment (map-get? milestone-payments {gig-id: gig-id, milestone-id: milestone-id})))
        (asserts! (is-eq tx-sender (get client gig)) err-unauthorized)
        (asserts! (>= (get status milestone) u2) err-milestone-not-approved)
        (asserts! (is-none existing-payment) err-escrow-exists)
        
        ;; Transfer payment to contract escrow
        (try! (stx-transfer? (get payment-amount milestone) tx-sender (as-contract tx-sender)))
        
        ;; Record payment
        (map-set milestone-payments {gig-id: gig-id, milestone-id: milestone-id}
            {
                client: tx-sender,
                freelancer: (get freelancer gig),
                amount: (get payment-amount milestone),
                escrowed: true,
                released: false,
                created-height: burn-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (release-milestone-payment (gig-id uint) (milestone-id uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (milestone (unwrap! (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id}) err-milestone-not-found))
         (payment (unwrap! (map-get? milestone-payments {gig-id: gig-id, milestone-id: milestone-id}) err-escrow-not-found)))
        (asserts! (is-eq tx-sender (get client gig)) err-unauthorized)
        (asserts! (is-eq (get status milestone) u3) err-milestone-not-approved)
        (asserts! (get escrowed payment) err-escrow-not-found)
        (asserts! (not (get released payment)) err-milestone-payment-released)
        
        ;; Release payment to freelancer
        (try! (as-contract (stx-transfer? (get amount payment) tx-sender (get freelancer payment))))
        
        ;; Update payment record
        (map-set milestone-payments {gig-id: gig-id, milestone-id: milestone-id}
            (merge payment {
                released: true
            })
        )
        
        ;; Update milestone status
        (map-set gig-milestones {gig-id: gig-id, milestone-id: milestone-id}
            (merge milestone {
                status: u4, ;; paid
                paid-height: burn-block-height
            })
        )
        
        ;; Update gig counter
        (let
            ((counter (unwrap! (map-get? gig-milestone-counters gig-id) err-not-found)))
            (map-set gig-milestone-counters gig-id
                (merge counter {
                    paid-milestone-value: (+ (get paid-milestone-value counter) (get amount payment))
                })
            )
        )
        
        (ok true)
    )
)

(define-public (update-milestone-deadline (gig-id uint) (milestone-id uint) (new-deadline-blocks uint))
    (let
        ((gig (unwrap! (map-get? gig-records gig-id) err-not-found))
         (milestone (unwrap! (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id}) err-milestone-not-found))
         (new-deadline (+ burn-block-height new-deadline-blocks)))
        (asserts! (is-eq tx-sender (get freelancer gig)) err-unauthorized)
        (asserts! (is-eq (get status milestone) u1) err-milestone-already-approved)
        (asserts! (> new-deadline-blocks u0) err-invalid-milestone-data)
        
        (map-set gig-milestones {gig-id: gig-id, milestone-id: milestone-id}
            (merge milestone {
                deadline-height: new-deadline
            })
        )
        
        (ok new-deadline)
    )
)

;; Read-only functions for milestone tracking
(define-read-only (get-milestone-details (gig-id uint) (milestone-id uint))
    (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id})
)

(define-read-only (get-gig-milestone-summary (gig-id uint))
    (map-get? gig-milestone-counters gig-id)
)

(define-read-only (get-milestone-payment-status (gig-id uint) (milestone-id uint))
    (map-get? milestone-payments {gig-id: gig-id, milestone-id: milestone-id})
)

(define-read-only (calculate-gig-progress (gig-id uint))
    (match (map-get? gig-milestone-counters gig-id)
        counter (ok {
            total-milestones: (get total-milestones counter),
            completed-milestones: (get completed-milestones counter),
            progress-percentage: (if (> (get total-milestones counter) u0)
                (/ (* (get completed-milestones counter) u100) (get total-milestones counter))
                u0
            ),
            total-value: (get total-milestone-value counter),
            paid-value: (get paid-milestone-value counter)
        })
        (ok {total-milestones: u0, completed-milestones: u0, progress-percentage: u0, total-value: u0, paid-value: u0})
    )
)

(define-read-only (is-milestone-overdue (gig-id uint) (milestone-id uint))
    (match (map-get? gig-milestones {gig-id: gig-id, milestone-id: milestone-id})
        milestone (ok {
            overdue: (and (< (get status milestone) u3) (> burn-block-height (get deadline-height milestone))),
            blocks-overdue: (if (> burn-block-height (get deadline-height milestone)) 
                (- burn-block-height (get deadline-height milestone)) u0)
        })
        err-milestone-not-found
    )
)

;; ============================================
;; REPUTATION ANALYTICS & PERFORMANCE METRICS
;; ============================================

(define-constant err-analytics-not-found (err u133))
(define-constant err-invalid-time-period (err u134))
(define-constant err-analytics-exists (err u135))
(define-constant err-insufficient-data (err u136))
(define-constant err-invalid-metric-type (err u137))

;; Performance metrics tracking
(define-map freelancer-analytics
    principal
    {
        total-earnings: uint,
        gigs-completed: uint,
        gigs-cancelled: uint,
        average-completion-time: uint,
        client-retention-rate: uint,
        streak-count: uint,
        best-streak: uint,
        last-activity-height: uint,
        performance-score: uint,
        reliability-index: uint
    }
)

;; Time-based performance periods (monthly snapshots)
(define-map performance-snapshots
    {freelancer: principal, period: uint}
    {
        period-start: uint,
        period-end: uint,
        gigs-completed: uint,
        total-earned: uint,
        average-rating: uint,
        completion-rate: uint,
        response-time: uint,
        client-satisfaction: uint
    }
)

;; Category-based performance tracking
(define-map category-performance
    {freelancer: principal, category: (string-ascii 64)}
    {
        gigs-completed: uint,
        total-earnings: uint,
        average-rating: uint,
        specialization-score: uint,
        category-rank: uint,
        last-updated: uint
    }
)

;; Achievement system
(define-map freelancer-achievements
    {freelancer: principal, achievement-id: uint}
    {
        title: (string-ascii 128),
        description: (string-ascii 256),
        tier: uint, ;; 1=bronze, 2=silver, 3=gold, 4=platinum
        earned-date: uint,
        category: (string-ascii 64),
        points: uint
    }
)

(define-map achievement-templates
    uint ;; achievement-id
    {
        title: (string-ascii 128),
        description: (string-ascii 256),
        requirements: (string-ascii 256),
        tier: uint,
        category: (string-ascii 64),
        points: uint,
        active: bool
    }
)

(define-data-var achievement-counter uint u0)
(define-data-var performance-period-blocks uint u1008) ;; ~1 week in blocks
(define-data-var min-gigs-for-analytics uint u3)

;; Initialize analytics for a freelancer
(define-public (initialize-analytics (freelancer principal))
    (begin
        (asserts! (is-some (map-get? freelancer-profiles freelancer)) err-not-found)
        (asserts! (is-none (map-get? freelancer-analytics freelancer)) err-analytics-exists)
        (ok (map-set freelancer-analytics freelancer
            {
                total-earnings: u0,
                gigs-completed: u0,
                gigs-cancelled: u0,
                average-completion-time: u0,
                client-retention-rate: u0,
                streak-count: u0,
                best-streak: u0,
                last-activity-height: burn-block-height,
                performance-score: u0,
                reliability-index: u0
            }
        ))
    )
)

;; Update analytics when gig is completed
(define-public (update-gig-analytics (freelancer principal) (gig-id uint) (earnings uint) (completion-time uint) (category (string-ascii 64)))
    (let
        ((analytics (default-to 
            {total-earnings: u0, gigs-completed: u0, gigs-cancelled: u0, average-completion-time: u0, 
             client-retention-rate: u0, streak-count: u0, best-streak: u0, last-activity-height: u0, 
             performance-score: u0, reliability-index: u0}
            (map-get? freelancer-analytics freelancer)))
         (new-completed (+ (get gigs-completed analytics) u1))
         (new-earnings (+ (get total-earnings analytics) earnings))
         (new-avg-time (/ (+ (* (get average-completion-time analytics) (get gigs-completed analytics)) completion-time) new-completed))
         (new-streak (+ (get streak-count analytics) u1))
         (new-best-streak (if (> new-streak (get best-streak analytics)) new-streak (get best-streak analytics))))
        
        ;; Update main analytics
        (map-set freelancer-analytics freelancer
            (merge analytics {
                total-earnings: new-earnings,
                gigs-completed: new-completed,
                average-completion-time: new-avg-time,
                streak-count: new-streak,
                best-streak: new-best-streak,
                last-activity-height: burn-block-height,
                performance-score: (calculate-performance-score new-completed new-avg-time new-streak),
                reliability-index: (calculate-reliability-index new-completed (get gigs-cancelled analytics))
            })
        )
        
        ;; Update category performance
        (try! (update-category-performance freelancer category earnings))
        
        ;; Check for achievements
        (unwrap-panic (check-and-award-achievements freelancer new-completed new-earnings new-streak))
        
        (ok true)
    )
)

;; Update category-specific performance
(define-private (update-category-performance (freelancer principal) (category (string-ascii 64)) (earnings uint))
    (let
        ((cat-perf (default-to 
            {gigs-completed: u0, total-earnings: u0, average-rating: u0, specialization-score: u0, category-rank: u0, last-updated: u0}
            (map-get? category-performance {freelancer: freelancer, category: category})))
         (profile (unwrap! (map-get? freelancer-profiles freelancer) err-not-found))
         (new-gigs (+ (get gigs-completed cat-perf) u1))
         (new-earnings (+ (get total-earnings cat-perf) earnings))
         (current-rating (if (> (get reviews-count profile) u0) 
                           (/ (get total-score profile) (get reviews-count profile)) u0))
         (spec-score (calculate-specialization-score new-gigs new-earnings current-rating)))
        
        (ok (map-set category-performance {freelancer: freelancer, category: category}
            {
                gigs-completed: new-gigs,
                total-earnings: new-earnings,
                average-rating: current-rating,
                specialization-score: spec-score,
                category-rank: u0, ;; Will be calculated in batch process
                last-updated: burn-block-height
            }
        ))
    )
)

;; Calculate performance score
(define-private (calculate-performance-score (completed uint) (avg-time uint) (streak uint))
    (let
        ((completion-factor (if (< (* completed u2) u50) (* completed u2) u50))
         (efficiency-factor (if (> avg-time u0) 
                              (let ((calc (/ u30000 avg-time))) 
                                (if (< calc u30) calc u30)) u0))
         (consistency-factor (if (< streak u20) streak u20)))
        (+ completion-factor efficiency-factor consistency-factor)
    )
)

;; Calculate reliability index
(define-private (calculate-reliability-index (completed uint) (cancelled uint))
    (if (is-eq (+ completed cancelled) u0)
        u100
        (/ (* completed u100) (+ completed cancelled))
    )
)

;; Calculate specialization score
(define-private (calculate-specialization-score (gigs uint) (earnings uint) (rating uint))
    (let
        ((volume-score (* gigs u10))
         (value-score (/ earnings u100000))
         (quality-score (* rating u20)))
        (+ volume-score value-score quality-score)
    )
)

;; Create performance snapshot for a period
(define-public (create-performance-snapshot (freelancer principal) (period uint))
    (let
        ((analytics (unwrap! (map-get? freelancer-analytics freelancer) err-analytics-not-found))
         (profile (unwrap! (map-get? freelancer-profiles freelancer) err-not-found))
         (period-start (* period (var-get performance-period-blocks)))
         (period-end (+ period-start (var-get performance-period-blocks))))
        
        (asserts! (>= (get gigs-completed analytics) (var-get min-gigs-for-analytics)) err-insufficient-data)
        
        (map-set performance-snapshots {freelancer: freelancer, period: period}
            {
                period-start: period-start,
                period-end: period-end,
                gigs-completed: (get gigs-completed analytics),
                total-earned: (get total-earnings analytics),
                average-rating: (if (> (get reviews-count profile) u0) 
                                  (/ (get total-score profile) (get reviews-count profile)) u0),
                completion-rate: (get reliability-index analytics),
                response-time: (get average-completion-time analytics),
                client-satisfaction: (get performance-score analytics)
            }
        )
        
        (ok true)
    )
)

;; Setup achievement templates (contract owner only)
(define-public (setup-achievement-template (title (string-ascii 128)) (description (string-ascii 256)) (requirements (string-ascii 256)) (tier uint) (category (string-ascii 64)) (points uint))
    (let
        ((new-id (+ (var-get achievement-counter) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (<= tier u4) (> tier u0) (> points u0)) err-invalid-metric-type)
        
        (map-set achievement-templates new-id
            {
                title: title,
                description: description,
                requirements: requirements,
                tier: tier,
                category: category,
                points: points,
                active: true
            }
        )
        
        (var-set achievement-counter new-id)
        (ok new-id)
    )
)

;; Check and award achievements
(define-private (check-and-award-achievements (freelancer principal) (completed-gigs uint) (total-earnings uint) (streak uint))
    (begin
        ;; Award "First Gig" achievement
        (if (and (is-eq completed-gigs u1) (is-none (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u1})))
            (unwrap-panic (award-achievement freelancer u1 "First Gig Completed" "Successfully completed your first gig" u1 "milestone" u10))
            true
        )
        
        ;; Award "Reliable Freelancer" achievement for 10 completed gigs
        (if (and (>= completed-gigs u10) (is-none (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u2})))
            (unwrap-panic (award-achievement freelancer u2 "Reliable Freelancer" "Completed 10 gigs successfully" u2 "reliability" u50))
            true
        )
        
        ;; Award "High Earner" achievement for 1M+ microSTX earnings
        (if (and (>= total-earnings u1000000) (is-none (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u3})))
            (unwrap-panic (award-achievement freelancer u3 "High Earner" "Earned over 1 STX in total" u3 "earnings" u100))
            true
        )
        
        ;; Award "Consistent Performer" achievement for 5+ streak
        (if (and (>= streak u5) (is-none (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u4})))
            (unwrap-panic (award-achievement freelancer u4 "Consistent Performer" "Maintained a streak of 5+ gigs" u2 "consistency" u30))
            true
        )
        
        (ok true)
    )
)

;; Award achievement
(define-private (award-achievement (freelancer principal) (achievement-id uint) (title (string-ascii 128)) (description (string-ascii 256)) (tier uint) (category (string-ascii 64)) (points uint))
    (begin
        (map-set freelancer-achievements {freelancer: freelancer, achievement-id: achievement-id}
            {
                title: title,
                description: description,
                tier: tier,
                earned-date: burn-block-height,
                category: category,
                points: points
            }
        )
        (ok true)
    )
)

;; Get achievement count for freelancer
(define-private (get-achievement-count (freelancer principal))
    ;; Simplified count - in production, this would iterate through all achievements
    (let
        ((ach1 (is-some (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u1})))
         (ach2 (is-some (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u2})))
         (ach3 (is-some (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u3})))
         (ach4 (is-some (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: u4}))))
        (+ (if ach1 u1 u0) (if ach2 u1 u0) (if ach3 u1 u0) (if ach4 u1 u0))
    )
)

;; Read-only functions for analytics
(define-read-only (get-freelancer-analytics (freelancer principal))
    (map-get? freelancer-analytics freelancer)
)

(define-read-only (get-performance-snapshot (freelancer principal) (period uint))
    (map-get? performance-snapshots {freelancer: freelancer, period: period})
)

(define-read-only (get-category-performance (freelancer principal) (category (string-ascii 64)))
    (map-get? category-performance {freelancer: freelancer, category: category})
)

(define-read-only (get-achievement (freelancer principal) (achievement-id uint))
    (map-get? freelancer-achievements {freelancer: freelancer, achievement-id: achievement-id})
)

(define-read-only (get-achievement-template (achievement-id uint))
    (map-get? achievement-templates achievement-id)
)

(define-read-only (get-comprehensive-analytics (freelancer principal))
    (match (map-get? freelancer-analytics freelancer)
        analytics (match (map-get? freelancer-profiles freelancer)
            profile (ok {
                ;; Core metrics
                total-earnings: (get total-earnings analytics),
                gigs-completed: (get gigs-completed analytics),
                gigs-cancelled: (get gigs-cancelled analytics),
                completion-rate: (get reliability-index analytics),
                
                ;; Performance metrics
                average-completion-time: (get average-completion-time analytics),
                performance-score: (get performance-score analytics),
                current-streak: (get streak-count analytics),
                best-streak: (get best-streak analytics),
                
                ;; Reputation metrics
                reputation-tier: (get tier-level profile),
                total-reviews: (get reviews-count profile),
                average-rating: (if (> (get reviews-count profile) u0) 
                                  (/ (get total-score profile) (get reviews-count profile)) u0),
                
                ;; Activity metrics
                last-activity: (get last-activity-height analytics),
                client-retention-rate: (get client-retention-rate analytics),
                
                ;; Achievement count
                total-achievements: (get-achievement-count freelancer)
            })
            err-not-found
        )
        err-analytics-not-found
    )
)

(define-read-only (calculate-freelancer-rank (freelancer principal))
    (match (map-get? freelancer-analytics freelancer)
        analytics (ok {
            performance-rank: (get performance-score analytics),
            earnings-rank: (/ (get total-earnings analytics) u100000),
            reliability-rank: (get reliability-index analytics),
            experience-rank: (get gigs-completed analytics),
            overall-rank: (+ (get performance-score analytics) 
                           (/ (get total-earnings analytics) u100000)
                           (get reliability-index analytics)
                           (get gigs-completed analytics))
        })
        err-analytics-not-found
    )
)

(define-read-only (get-performance-trends (freelancer principal) (periods uint))
    (let
        ((current-period (/ burn-block-height (var-get performance-period-blocks))))
        ;; Return performance data for last N periods
        ;; This is a simplified version - production would aggregate multiple periods
        (match (map-get? performance-snapshots {freelancer: freelancer, period: current-period})
            snapshot (ok {
                current-period: current-period,
                gigs-completed: (get gigs-completed snapshot),
                total-earned: (get total-earned snapshot),
                average-rating: (get average-rating snapshot),
                completion-rate: (get completion-rate snapshot)
            })
            (ok {current-period: current-period, gigs-completed: u0, total-earned: u0, average-rating: u0, completion-rate: u0})
        )
    )
)

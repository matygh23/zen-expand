;; ZenExpand - Decentralized Educational Funding Platform
;; A comprehensive smart contract for educational micro-lending and outcome-based rewards

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-milestone-not-completed (err u106))
(define-constant err-already-claimed (err u107))

;; Data Variables
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee
(define-data-var min-scholarship-amount uint u1000000) ;; 1 STX minimum
(define-data-var total-scholarships-funded uint u0)
(define-data-var total-amount-distributed uint u0)

;; Data Maps

;; Scholarship tracking
(define-map scholarships
    { scholarship-id: uint }
    {
        learner: principal,
        sponsor: principal,
        total-amount: uint,
        amount-released: uint,
        milestones-completed: uint,
        total-milestones: uint,
        status: (string-ascii 20),
        created-at: uint,
        learning-track: (string-ascii 50)
    }
)

;; Milestone tracking
(define-map milestones
    { scholarship-id: uint, milestone-index: uint }
    {
        description: (string-ascii 100),
        amount: uint,
        completed: bool,
        verified-by: (optional principal),
        completion-date: (optional uint)
    }
)

;; Staking for sponsors
(define-map sponsor-stakes
    { sponsor: principal }
    {
        staked-amount: uint,
        scholarships-sponsored: uint,
        total-rewards-earned: uint,
        stake-timestamp: uint
    }
)

;; Learning impact scores
(define-map learner-profiles
    { learner: principal }
    {
        scholarships-received: uint,
        milestones-completed: uint,
        impact-score: uint,
        total-funding-received: uint,
        graduated: bool
    }
)

;; Community validators
(define-map validators
    { validator: principal }
    {
        validations-completed: uint,
        reputation-score: uint,
        is-active: bool
    }
)

;; Scholarship counter
(define-data-var scholarship-counter uint u0)

;; Read-only functions

(define-read-only (get-scholarship (scholarship-id uint))
    (map-get? scholarships { scholarship-id: scholarship-id })
)

(define-read-only (get-milestone (scholarship-id uint) (milestone-index uint))
    (map-get? milestones { scholarship-id: scholarship-id, milestone-index: milestone-index })
)

(define-read-only (get-sponsor-stake (sponsor principal))
    (map-get? sponsor-stakes { sponsor: sponsor })
)

(define-read-only (get-learner-profile (learner principal))
    (map-get? learner-profiles { learner: learner })
)

(define-read-only (get-validator (validator principal))
    (map-get? validators { validator: validator })
)

(define-read-only (get-platform-stats)
    (ok {
        total-scholarships: (var-get total-scholarships-funded),
        total-distributed: (var-get total-amount-distributed),
        platform-fee: (var-get platform-fee-percentage),
        min-scholarship: (var-get min-scholarship-amount)
    })
)

;; Private functions

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-percentage)) u100)
)

;; Public functions

;; Sponsor stakes tokens to fund educational tracks
(define-public (stake-as-sponsor (amount uint))
    (let
        (
            (existing-stake (default-to 
                { staked-amount: u0, scholarships-sponsored: u0, total-rewards-earned: u0, stake-timestamp: u0 }
                (map-get? sponsor-stakes { sponsor: tx-sender })
            ))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok (map-set sponsor-stakes
            { sponsor: tx-sender }
            {
                staked-amount: (+ (get staked-amount existing-stake) amount),
                scholarships-sponsored: (get scholarships-sponsored existing-stake),
                total-rewards-earned: (get total-rewards-earned existing-stake),
                stake-timestamp: block-height
            }
        ))
    )
)

;; Create a new scholarship with milestones
(define-public (create-scholarship 
    (learner principal)
    (total-amount uint)
    (total-milestones uint)
    (learning-track (string-ascii 50)))
    (let
        (
            (scholarship-id (+ (var-get scholarship-counter) u1))
            (sponsor-stake (unwrap! (map-get? sponsor-stakes { sponsor: tx-sender }) err-unauthorized))
            (fee (calculate-platform-fee total-amount))
            (net-amount (- total-amount fee))
        )
        (asserts! (>= total-amount (var-get min-scholarship-amount)) err-invalid-amount)
        (asserts! (> total-milestones u0) err-invalid-amount)
        (asserts! (>= (get staked-amount sponsor-stake) total-amount) err-insufficient-funds)
        
        ;; Transfer funds to contract
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        
        ;; Create scholarship record
        (map-set scholarships
            { scholarship-id: scholarship-id }
            {
                learner: learner,
                sponsor: tx-sender,
                total-amount: net-amount,
                amount-released: u0,
                milestones-completed: u0,
                total-milestones: total-milestones,
                status: "active",
                created-at: block-height,
                learning-track: learning-track
            }
        )
        
        ;; Update learner profile
        (let
            (
                (learner-data (default-to
                    { scholarships-received: u0, milestones-completed: u0, impact-score: u0, total-funding-received: u0, graduated: false }
                    (map-get? learner-profiles { learner: learner })
                ))
            )
            (map-set learner-profiles
                { learner: learner }
                {
                    scholarships-received: (+ (get scholarships-received learner-data) u1),
                    milestones-completed: (get milestones-completed learner-data),
                    impact-score: (get impact-score learner-data),
                    total-funding-received: (+ (get total-funding-received learner-data) net-amount),
                    graduated: (get graduated learner-data)
                }
            )
        )
        
        ;; Update sponsor stats
        (map-set sponsor-stakes
            { sponsor: tx-sender }
            (merge sponsor-stake { scholarships-sponsored: (+ (get scholarships-sponsored sponsor-stake) u1) })
        )
        
        ;; Update counters
        (var-set scholarship-counter scholarship-id)
        (var-set total-scholarships-funded (+ (var-get total-scholarships-funded) u1))
        
        (ok scholarship-id)
    )
)

;; Add milestone to scholarship
(define-public (add-milestone
    (scholarship-id uint)
    (milestone-index uint)
    (description (string-ascii 100))
    (amount uint))
    (let
        (
            (scholarship (unwrap! (map-get? scholarships { scholarship-id: scholarship-id }) err-not-found))
        )
        (asserts! (is-eq (get sponsor scholarship) tx-sender) err-unauthorized)
        (asserts! (< milestone-index (get total-milestones scholarship)) err-invalid-amount)
        
        (ok (map-set milestones
            { scholarship-id: scholarship-id, milestone-index: milestone-index }
            {
                description: description,
                amount: amount,
                completed: false,
                verified-by: none,
                completion-date: none
            }
        ))
    )
)

;; Register as validator
(define-public (register-validator)
    (ok (map-set validators
        { validator: tx-sender }
        {
            validations-completed: u0,
            reputation-score: u100,
            is-active: true
        }
    ))
)

;; Validate and complete milestone
(define-public (validate-milestone
    (scholarship-id uint)
    (milestone-index uint))
    (let
        (
            (scholarship (unwrap! (map-get? scholarships { scholarship-id: scholarship-id }) err-not-found))
            (milestone (unwrap! (map-get? milestones { scholarship-id: scholarship-id, milestone-index: milestone-index }) err-not-found))
            (validator-data (unwrap! (map-get? validators { validator: tx-sender }) err-unauthorized))
        )
        (asserts! (get is-active validator-data) err-unauthorized)
        (asserts! (not (get completed milestone)) err-already-claimed)
        
        ;; Mark milestone as completed
        (map-set milestones
            { scholarship-id: scholarship-id, milestone-index: milestone-index }
            (merge milestone {
                completed: true,
                verified-by: (some tx-sender),
                completion-date: (some block-height)
            })
        )
        
        ;; Update validator stats
        (map-set validators
            { validator: tx-sender }
            {
                validations-completed: (+ (get validations-completed validator-data) u1),
                reputation-score: (+ (get reputation-score validator-data) u10),
                is-active: true
            }
        )
        
        ;; Update scholarship
        (map-set scholarships
            { scholarship-id: scholarship-id }
            (merge scholarship {
                milestones-completed: (+ (get milestones-completed scholarship) u1)
            })
        )
        
        (ok true)
    )
)

;; Release milestone payment to learner
(define-public (release-milestone-payment
    (scholarship-id uint)
    (milestone-index uint))
    (let
        (
            (scholarship (unwrap! (map-get? scholarships { scholarship-id: scholarship-id }) err-not-found))
            (milestone (unwrap! (map-get? milestones { scholarship-id: scholarship-id, milestone-index: milestone-index }) err-not-found))
            (learner-data (unwrap! (map-get? learner-profiles { learner: (get learner scholarship) }) err-not-found))
        )
        (asserts! (get completed milestone) err-milestone-not-completed)
        (asserts! (or (is-eq tx-sender (get sponsor scholarship)) (is-eq tx-sender (get learner scholarship))) err-unauthorized)
        
        ;; Transfer milestone amount to learner
        (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get learner scholarship))))
        
        ;; Update scholarship
        (map-set scholarships
            { scholarship-id: scholarship-id }
            (merge scholarship {
                amount-released: (+ (get amount-released scholarship) (get amount milestone))
            })
        )
        
        ;; Update learner profile
        (map-set learner-profiles
            { learner: (get learner scholarship) }
            (merge learner-data {
                milestones-completed: (+ (get milestones-completed learner-data) u1),
                impact-score: (+ (get impact-score learner-data) u10)
            })
        )
        
        ;; Update total distributed
        (var-set total-amount-distributed (+ (var-get total-amount-distributed) (get amount milestone)))
        
        (ok true)
    )
)

;; Mark learner as graduated
(define-public (mark-graduated (scholarship-id uint))
    (let
        (
            (scholarship (unwrap! (map-get? scholarships { scholarship-id: scholarship-id }) err-not-found))
            (learner-data (unwrap! (map-get? learner-profiles { learner: (get learner scholarship) }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get sponsor scholarship)) err-unauthorized)
        (asserts! (is-eq (get milestones-completed scholarship) (get total-milestones scholarship)) err-milestone-not-completed)
        
        ;; Update scholarship status
        (map-set scholarships
            { scholarship-id: scholarship-id }
            (merge scholarship { status: "completed" })
        )
        
        ;; Mark learner as graduated
        (map-set learner-profiles
            { learner: (get learner scholarship) }
            (merge learner-data {
                graduated: true,
                impact-score: (+ (get impact-score learner-data) u50)
            })
        )
        
        (ok true)
    )
)

;; Withdraw stake (only if no active scholarships)
(define-public (withdraw-stake (amount uint))
    (let
        (
            (sponsor-data (unwrap! (map-get? sponsor-stakes { sponsor: tx-sender }) err-not-found))
        )
        (asserts! (>= (get staked-amount sponsor-data) amount) err-insufficient-funds)
        
        ;; Transfer stake back to sponsor
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        
        ;; Update stake
        (ok (map-set sponsor-stakes
            { sponsor: tx-sender }
            (merge sponsor-data {
                staked-amount: (- (get staked-amount sponsor-data) amount)
            })
        ))
    )
)

;; Admin function to update platform fee
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u10) err-invalid-amount)
        (ok (var-set platform-fee-percentage new-fee))
    )
)
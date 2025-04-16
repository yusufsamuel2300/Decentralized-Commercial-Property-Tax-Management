;; Assessment Tracking Contract
;; Monitors official tax valuations

;; Define data maps
(define-map assessments
  { property-id: uint, year: uint }
  {
    assessed-value: uint,
    tax-rate: uint,
    tax-amount: uint,
    assessor: principal,
    assessment-date: uint
  }
)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-ASSESSMENT-EXISTS u101)
(define-constant ERR-ASSESSMENT-NOT-FOUND u102)
(define-constant ERR-INVALID-YEAR u103)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Authorized assessors
(define-map authorized-assessors
  { assessor: principal }
  { authorized: bool }
)

;; Read-only functions
(define-read-only (get-assessment (property-id uint) (year uint))
  (map-get? assessments { property-id: property-id, year: year })
)

(define-read-only (is-authorized-assessor (assessor principal))
  (default-to false (get authorized (map-get? authorized-assessors { assessor: assessor })))
)

;; Public functions
(define-public (add-authorized-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
    (map-set authorized-assessors { assessor: assessor } { authorized: true })
    (ok true)
  )
)

(define-public (remove-authorized-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
    (map-set authorized-assessors { assessor: assessor } { authorized: false })
    (ok true)
  )
)

(define-public (record-assessment
                (property-id uint)
                (year uint)
                (assessed-value uint)
                (tax-rate uint))
  (let ((current-time (unwrap-panic (get-block-info? time u0)))
        (tax-amount (/ (* assessed-value tax-rate) u10000)))

    ;; Check authorization
    (asserts! (is-authorized-assessor tx-sender) (err ERR-NOT-AUTHORIZED))

    ;; Validate year
    (asserts! (> year u2000) (err ERR-INVALID-YEAR))

    ;; Record the assessment
    (map-set assessments
      { property-id: property-id, year: year }
      {
        assessed-value: assessed-value,
        tax-rate: tax-rate,
        tax-amount: tax-amount,
        assessor: tx-sender,
        assessment-date: current-time
      }
    )

    (ok tax-amount)
  )
)

(define-public (update-assessment
                (property-id uint)
                (year uint)
                (assessed-value uint)
                (tax-rate uint))
  (let ((assessment (map-get? assessments { property-id: property-id, year: year })))
    ;; Check if assessment exists
    (asserts! (is-some assessment) (err ERR-ASSESSMENT-NOT-FOUND))

    ;; Check authorization
    (asserts! (is-authorized-assessor tx-sender) (err ERR-NOT-AUTHORIZED))

    ;; Calculate new tax amount
    (let ((tax-amount (/ (* assessed-value tax-rate) u10000))
          (current-time (unwrap-panic (get-block-info? time u0))))

      ;; Update the assessment
      (map-set assessments
        { property-id: property-id, year: year }
        {
          assessed-value: assessed-value,
          tax-rate: tax-rate,
          tax-amount: tax-amount,
          assessor: tx-sender,
          assessment-date: current-time
        }
      )

      (ok tax-amount)
    )
  )
)

;; Property Registration Contract
;; Records details and valuation of commercial buildings

;; Define data maps
(define-map properties
  { property-id: uint }
  {
    owner: principal,
    address: (string-utf8 256),
    square-footage: uint,
    construction-year: uint,
    initial-valuation: uint,
    registration-date: uint
  }
)

(define-map property-owners
  { owner: principal }
  { property-count: uint }
)

(define-data-var next-property-id uint u1)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-PROPERTY-EXISTS u101)
(define-constant ERR-PROPERTY-NOT-FOUND u102)

;; Read-only functions
(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-owner-property-count (owner principal))
  (default-to { property-count: u0 } (map-get? property-owners { owner: owner }))
)

;; Public functions
(define-public (register-property
                (address (string-utf8 256))
                (square-footage uint)
                (construction-year uint)
                (initial-valuation uint))
  (let ((property-id (var-get next-property-id))
        (current-time (unwrap-panic (get-block-info? time u0))))

    ;; Register the property
    (map-set properties
      { property-id: property-id }
      {
        owner: tx-sender,
        address: address,
        square-footage: square-footage,
        construction-year: construction-year,
        initial-valuation: initial-valuation,
        registration-date: current-time
      }
    )

    ;; Update owner's property count
    (let ((owner-data (get-owner-property-count tx-sender)))
      (map-set property-owners
        { owner: tx-sender }
        { property-count: (+ u1 (get property-count owner-data)) }
      )
    )

    ;; Increment property ID counter
    (var-set next-property-id (+ property-id u1))

    ;; Return success with property ID
    (ok property-id)
  )
)

(define-public (update-property-valuation (property-id uint) (new-valuation uint))
  (let ((property (map-get? properties { property-id: property-id })))
    (asserts! (is-some property) (err ERR-PROPERTY-NOT-FOUND))
    (asserts! (is-eq tx-sender (get owner (unwrap-panic property))) (err ERR-NOT-AUTHORIZED))

    (map-set properties
      { property-id: property-id }
      (merge (unwrap-panic property) { initial-valuation: new-valuation })
    )

    (ok true)
  )
)

(define-public (transfer-property (property-id uint) (new-owner principal))
  (let ((property (map-get? properties { property-id: property-id })))
    (asserts! (is-some property) (err ERR-PROPERTY-NOT-FOUND))
    (asserts! (is-eq tx-sender (get owner (unwrap-panic property))) (err ERR-NOT-AUTHORIZED))

    ;; Update property ownership
    (map-set properties
      { property-id: property-id }
      (merge (unwrap-panic property) { owner: new-owner })
    )

    ;; Decrease previous owner's count
    (let ((prev-owner-data (get-owner-property-count tx-sender)))
      (map-set property-owners
        { owner: tx-sender }
        { property-count: (- (get property-count prev-owner-data) u1) }
      )
    )

    ;; Increase new owner's count
    (let ((new-owner-data (get-owner-property-count new-owner)))
      (map-set property-owners
        { owner: new-owner }
        { property-count: (+ (get property-count new-owner-data) u1) }
      )
    )

    (ok true)
  )
)

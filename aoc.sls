;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2024 Jasper Reef
;; SPDX-License-Identifier: MIT
#!r6rs

(library (aoc)
  (export hello)
  (import (rnrs))

(define (hello whom)
  (string-append "Hello " whom "!")))

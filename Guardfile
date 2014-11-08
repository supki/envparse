repl_options =
  [ "--ghc-options=-ignore-dot-ghci" \
  ]

guard :haskell, all_on_start: true, all_on_pass: true, repl_options: repl_options do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
  watch(%r{\.cabal$})
end

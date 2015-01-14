cmd = "cabal repl spec --ghc-options='-ignore-dot-ghci -DTEST'"

guard :haskell, all_on_start: true, all_on_pass: true, cmd: cmd do
    watch(%r{test/.+Spec\.l?hs$})
    watch(%r{src/.+\.l?hs$})
    watch(%r{\.cabal$})
end

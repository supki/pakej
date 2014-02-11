guard :haskell, ghci_options: ["-ignore-dot-ghci", "-idist/build/autogen", "-DTEST"], all_on_start: true do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end

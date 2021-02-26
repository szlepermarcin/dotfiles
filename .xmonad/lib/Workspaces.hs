module Workspaces (myWorkspaces) where

xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
  $ [ "\61612"
    , "\61899"
    , "\61947"
    , "\61635"
    , "\61502"
    , "\61501"
    , "\61705"
    , "\61564"
    , "\62150"
    , "\61872"
    ]
  where
    clickable l =
      [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>"
      | (i, ws) <- zip [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] l
      , let n = i
      ]


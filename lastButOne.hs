lastButOne :: [a] -> Maybe a
lastButOne xs =
    if length xs <= 1
    then Nothing
    else Just (last (init xs))

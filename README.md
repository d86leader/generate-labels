# generate-labels
Overlapping record field names are great, but you can rarely use the
fields as accessor functions, because GHC complains about overlaps.
OverloadedLabels to the rescue! With them GHC only complains about being
unable to infer types. But with OverloadedLabels the problem is that you
have to write instances by hand. This is no longer a problem with this
TemplateHaskell module.

## Usage:

```
-- Required language extensions
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.OverloadedLabels.TH (generateLabels)

data MyRecord = MyRecord {foo :: Integer, bar :: String}
newtype OtherRecord = OtherRecord {bar :: String}

generateLabels ''MyRecord
generateLabels ''OtherRecord

main =
    let x = MyRecord {foo = 0, bar = "hello"}
        y = OtherRecord ", world"
    in putStrLn $ #bar x <> #bar y
```

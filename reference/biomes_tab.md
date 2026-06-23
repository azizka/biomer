# Tabulate the number of occurrences per biome

Summarizes the number of **occurrence records** (one row of `x` = one
occurrence) in each biome, for one or more biome layers. The output is a
long-format table with one row per (layer, biome) pair.

## Usage

``` r
biomes_tab(x, value = "names")
```

## Arguments

- x:

  A data frame returned by
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md).

- value:

  Character. `"names"` (default) tabulates the `_name` columns from
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md);
  `"ID"` tabulates the `_value` columns.

## Value

A data frame with columns `layer`, `biome`, and `n` (the number of
occurrence records in that biome on that layer).

## Details

This function counts occurrences, not species. To count unique species
per biome, deduplicate by species before tabulating (e.g.
`dplyr::distinct(species, biome)` after combining classifications with
the original data).

## Examples

``` r
# Load example occurrence data
data("biomes_example")

# \donttest{
# biomes_classify() downloads and caches the biome raster (~36 MB).

# Tabulate by biome name
classified_names <- biomes_classify(
  x     = biomes_example,
  value = "name"
)
#> no biome file or layer provided using default biomes
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 31 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
#>   - Biome_Inventory_layer_02 (Buchhorn et al., 2019)
#>   - Biome_Inventory_layer_03 (Beck et al., 2018)
#>   - Biome_Inventory_layer_04 (Hengl et al., 2018)
#>   - Biome_Inventory_layer_05 (Dinerstein et al., 2017)
#>   - Biome_Inventory_layer_06 (Zhang et al., 2017)
#>   - Biome_Inventory_layer_07 (Netzel & Stepinski, 2016a)
#>   - Biome_Inventory_layer_08 (Netzel & Stepinski, 2016b)
#>   - Biome_Inventory_layer_09 (Higgins et al., 2016)
#>   - Biome_Inventory_layer_10 (Pfadenhauer & Klötzli, 2014)
#>   - Biome_Inventory_layer_11 (Zhang & Yan, 2014)
#>   - Biome_Inventory_layer_12 (Metzger et al., 2013)
#>   - Biome_Inventory_layer_13 (Food and Agriculture Organization of the United Nations, 2012)
#>   - Biome_Inventory_layer_14 (Tateishi et al., 2011; Tateishi et al., 2014; Kobayashi et al., 2017)
#>   - Biome_Inventory_layer_15 (Defries et al., 2010)
#>   - Biome_Inventory_layer_16 (Ellis et al., 2010)
#>   - Biome_Inventory_layer_17 (European Space Agency, 2010)
#>   - Biome_Inventory_layer_18 (Friedl et al., 2010)
#>   - Biome_Inventory_layer_19 (The Nature Conservancy, 2009)
#>   - Biome_Inventory_layer_20 (Peel et al., 2007)
#>   - Biome_Inventory_layer_21 (Bartholomé & Belward, 2005)
#>   - Biome_Inventory_layer_22 (Kaplan et al., 2003)
#>   - Biome_Inventory_layer_23 (Olson et al., 2001)
#>   - Biome_Inventory_layer_24 (Loveland et al., 2000)
#>   - Biome_Inventory_layer_25 (Ramankutty & Foley, 1999)
#>   - Biome_Inventory_layer_26 (Leemans, 1990)
#>   - Biome_Inventory_layer_27 (Schultz, 1988, 1995, 2002, 2008, 2016)
#>   - Biome_Inventory_layer_28 (Müller-Hohenstein, 1981)
#>   - Biome_Inventory_layer_29 (Schmithüsen, 1976)
#>   - Biome_Inventory_layer_30 (Whittaker, 1975)
#>   - Biome_Inventory_layer_31 (Walter, 1964, 1968; Walter & Breckle, 1970; Breckle & Rafiqpoor, 2019)
biomes_tab(classified_names, value = "names")
#>                        layer
#> 1   Biome_Inventory_layer_01
#> 2   Biome_Inventory_layer_01
#> 3   Biome_Inventory_layer_01
#> 4   Biome_Inventory_layer_01
#> 5   Biome_Inventory_layer_01
#> 6   Biome_Inventory_layer_01
#> 7   Biome_Inventory_layer_01
#> 8   Biome_Inventory_layer_01
#> 9   Biome_Inventory_layer_01
#> 10  Biome_Inventory_layer_01
#> 11  Biome_Inventory_layer_01
#> 12  Biome_Inventory_layer_01
#> 13  Biome_Inventory_layer_01
#> 14  Biome_Inventory_layer_01
#> 15  Biome_Inventory_layer_01
#> 16  Biome_Inventory_layer_01
#> 17  Biome_Inventory_layer_01
#> 18  Biome_Inventory_layer_01
#> 19  Biome_Inventory_layer_01
#> 20  Biome_Inventory_layer_01
#> 21  Biome_Inventory_layer_02
#> 22  Biome_Inventory_layer_02
#> 23  Biome_Inventory_layer_02
#> 24  Biome_Inventory_layer_02
#> 25  Biome_Inventory_layer_02
#> 26  Biome_Inventory_layer_02
#> 27  Biome_Inventory_layer_02
#> 28  Biome_Inventory_layer_02
#> 29  Biome_Inventory_layer_02
#> 30  Biome_Inventory_layer_02
#> 31  Biome_Inventory_layer_02
#> 32  Biome_Inventory_layer_02
#> 33  Biome_Inventory_layer_02
#> 34  Biome_Inventory_layer_02
#> 35  Biome_Inventory_layer_02
#> 36  Biome_Inventory_layer_02
#> 37  Biome_Inventory_layer_02
#> 38  Biome_Inventory_layer_02
#> 39  Biome_Inventory_layer_03
#> 40  Biome_Inventory_layer_03
#> 41  Biome_Inventory_layer_03
#> 42  Biome_Inventory_layer_03
#> 43  Biome_Inventory_layer_03
#> 44  Biome_Inventory_layer_03
#> 45  Biome_Inventory_layer_03
#> 46  Biome_Inventory_layer_03
#> 47  Biome_Inventory_layer_03
#> 48  Biome_Inventory_layer_03
#> 49  Biome_Inventory_layer_03
#> 50  Biome_Inventory_layer_03
#> 51  Biome_Inventory_layer_03
#> 52  Biome_Inventory_layer_03
#> 53  Biome_Inventory_layer_03
#> 54  Biome_Inventory_layer_03
#> 55  Biome_Inventory_layer_03
#> 56  Biome_Inventory_layer_03
#> 57  Biome_Inventory_layer_03
#> 58  Biome_Inventory_layer_03
#> 59  Biome_Inventory_layer_03
#> 60  Biome_Inventory_layer_03
#> 61  Biome_Inventory_layer_03
#> 62  Biome_Inventory_layer_03
#> 63  Biome_Inventory_layer_03
#> 64  Biome_Inventory_layer_03
#> 65  Biome_Inventory_layer_04
#> 66  Biome_Inventory_layer_04
#> 67  Biome_Inventory_layer_04
#> 68  Biome_Inventory_layer_04
#> 69  Biome_Inventory_layer_04
#> 70  Biome_Inventory_layer_04
#> 71  Biome_Inventory_layer_04
#> 72  Biome_Inventory_layer_04
#> 73  Biome_Inventory_layer_04
#> 74  Biome_Inventory_layer_04
#> 75  Biome_Inventory_layer_04
#> 76  Biome_Inventory_layer_04
#> 77  Biome_Inventory_layer_04
#> 78  Biome_Inventory_layer_04
#> 79  Biome_Inventory_layer_04
#> 80  Biome_Inventory_layer_04
#> 81  Biome_Inventory_layer_04
#> 82  Biome_Inventory_layer_04
#> 83  Biome_Inventory_layer_04
#> 84  Biome_Inventory_layer_04
#> 85  Biome_Inventory_layer_05
#> 86  Biome_Inventory_layer_05
#> 87  Biome_Inventory_layer_05
#> 88  Biome_Inventory_layer_05
#> 89  Biome_Inventory_layer_05
#> 90  Biome_Inventory_layer_05
#> 91  Biome_Inventory_layer_05
#> 92  Biome_Inventory_layer_05
#> 93  Biome_Inventory_layer_05
#> 94  Biome_Inventory_layer_05
#> 95  Biome_Inventory_layer_05
#> 96  Biome_Inventory_layer_05
#> 97  Biome_Inventory_layer_05
#> 98  Biome_Inventory_layer_05
#> 99  Biome_Inventory_layer_05
#> 100 Biome_Inventory_layer_05
#> 101 Biome_Inventory_layer_06
#> 102 Biome_Inventory_layer_06
#> 103 Biome_Inventory_layer_06
#> 104 Biome_Inventory_layer_06
#> 105 Biome_Inventory_layer_06
#> 106 Biome_Inventory_layer_06
#> 107 Biome_Inventory_layer_06
#> 108 Biome_Inventory_layer_06
#> 109 Biome_Inventory_layer_06
#> 110 Biome_Inventory_layer_06
#> 111 Biome_Inventory_layer_06
#> 112 Biome_Inventory_layer_06
#> 113 Biome_Inventory_layer_06
#> 114 Biome_Inventory_layer_06
#> 115 Biome_Inventory_layer_06
#> 116 Biome_Inventory_layer_07
#> 117 Biome_Inventory_layer_07
#> 118 Biome_Inventory_layer_07
#> 119 Biome_Inventory_layer_07
#> 120 Biome_Inventory_layer_07
#> 121 Biome_Inventory_layer_07
#> 122 Biome_Inventory_layer_07
#> 123 Biome_Inventory_layer_07
#> 124 Biome_Inventory_layer_07
#> 125 Biome_Inventory_layer_07
#> 126 Biome_Inventory_layer_07
#> 127 Biome_Inventory_layer_07
#> 128 Biome_Inventory_layer_07
#> 129 Biome_Inventory_layer_08
#> 130 Biome_Inventory_layer_08
#> 131 Biome_Inventory_layer_08
#> 132 Biome_Inventory_layer_08
#> 133 Biome_Inventory_layer_08
#> 134 Biome_Inventory_layer_08
#> 135 Biome_Inventory_layer_08
#> 136 Biome_Inventory_layer_08
#> 137 Biome_Inventory_layer_08
#> 138 Biome_Inventory_layer_08
#> 139 Biome_Inventory_layer_08
#> 140 Biome_Inventory_layer_08
#> 141 Biome_Inventory_layer_08
#> 142 Biome_Inventory_layer_08
#> 143 Biome_Inventory_layer_09
#> 144 Biome_Inventory_layer_09
#> 145 Biome_Inventory_layer_09
#> 146 Biome_Inventory_layer_09
#> 147 Biome_Inventory_layer_09
#> 148 Biome_Inventory_layer_09
#> 149 Biome_Inventory_layer_09
#> 150 Biome_Inventory_layer_09
#> 151 Biome_Inventory_layer_09
#> 152 Biome_Inventory_layer_09
#> 153 Biome_Inventory_layer_09
#> 154 Biome_Inventory_layer_09
#> 155 Biome_Inventory_layer_09
#> 156 Biome_Inventory_layer_09
#> 157 Biome_Inventory_layer_09
#> 158 Biome_Inventory_layer_09
#> 159 Biome_Inventory_layer_09
#> 160 Biome_Inventory_layer_09
#> 161 Biome_Inventory_layer_09
#> 162 Biome_Inventory_layer_09
#> 163 Biome_Inventory_layer_09
#> 164 Biome_Inventory_layer_09
#> 165 Biome_Inventory_layer_09
#> 166 Biome_Inventory_layer_09
#> 167 Biome_Inventory_layer_09
#> 168 Biome_Inventory_layer_10
#> 169 Biome_Inventory_layer_10
#> 170 Biome_Inventory_layer_10
#> 171 Biome_Inventory_layer_10
#> 172 Biome_Inventory_layer_10
#> 173 Biome_Inventory_layer_10
#> 174 Biome_Inventory_layer_10
#> 175 Biome_Inventory_layer_10
#> 176 Biome_Inventory_layer_10
#> 177 Biome_Inventory_layer_10
#> 178 Biome_Inventory_layer_10
#> 179 Biome_Inventory_layer_10
#> 180 Biome_Inventory_layer_10
#> 181 Biome_Inventory_layer_10
#> 182 Biome_Inventory_layer_10
#> 183 Biome_Inventory_layer_10
#> 184 Biome_Inventory_layer_10
#> 185 Biome_Inventory_layer_10
#> 186 Biome_Inventory_layer_10
#> 187 Biome_Inventory_layer_10
#> 188 Biome_Inventory_layer_10
#> 189 Biome_Inventory_layer_10
#> 190 Biome_Inventory_layer_10
#> 191 Biome_Inventory_layer_10
#> 192 Biome_Inventory_layer_10
#> 193 Biome_Inventory_layer_10
#> 194 Biome_Inventory_layer_10
#> 195 Biome_Inventory_layer_10
#> 196 Biome_Inventory_layer_10
#> 197 Biome_Inventory_layer_10
#> 198 Biome_Inventory_layer_10
#> 199 Biome_Inventory_layer_10
#> 200 Biome_Inventory_layer_11
#> 201 Biome_Inventory_layer_11
#> 202 Biome_Inventory_layer_11
#> 203 Biome_Inventory_layer_11
#> 204 Biome_Inventory_layer_11
#> 205 Biome_Inventory_layer_11
#> 206 Biome_Inventory_layer_11
#> 207 Biome_Inventory_layer_11
#> 208 Biome_Inventory_layer_11
#> 209 Biome_Inventory_layer_11
#> 210 Biome_Inventory_layer_11
#> 211 Biome_Inventory_layer_11
#> 212 Biome_Inventory_layer_11
#> 213 Biome_Inventory_layer_12
#> 214 Biome_Inventory_layer_12
#> 215 Biome_Inventory_layer_12
#> 216 Biome_Inventory_layer_12
#> 217 Biome_Inventory_layer_12
#> 218 Biome_Inventory_layer_12
#> 219 Biome_Inventory_layer_12
#> 220 Biome_Inventory_layer_12
#> 221 Biome_Inventory_layer_12
#> 222 Biome_Inventory_layer_12
#> 223 Biome_Inventory_layer_12
#> 224 Biome_Inventory_layer_12
#> 225 Biome_Inventory_layer_12
#> 226 Biome_Inventory_layer_12
#> 227 Biome_Inventory_layer_12
#> 228 Biome_Inventory_layer_12
#> 229 Biome_Inventory_layer_12
#> 230 Biome_Inventory_layer_13
#> 231 Biome_Inventory_layer_13
#> 232 Biome_Inventory_layer_13
#> 233 Biome_Inventory_layer_13
#> 234 Biome_Inventory_layer_13
#> 235 Biome_Inventory_layer_13
#> 236 Biome_Inventory_layer_13
#> 237 Biome_Inventory_layer_13
#> 238 Biome_Inventory_layer_13
#> 239 Biome_Inventory_layer_13
#> 240 Biome_Inventory_layer_13
#> 241 Biome_Inventory_layer_13
#> 242 Biome_Inventory_layer_13
#> 243 Biome_Inventory_layer_13
#> 244 Biome_Inventory_layer_13
#> 245 Biome_Inventory_layer_13
#> 246 Biome_Inventory_layer_13
#> 247 Biome_Inventory_layer_13
#> 248 Biome_Inventory_layer_13
#> 249 Biome_Inventory_layer_13
#> 250 Biome_Inventory_layer_13
#> 251 Biome_Inventory_layer_13
#> 252 Biome_Inventory_layer_14
#> 253 Biome_Inventory_layer_14
#> 254 Biome_Inventory_layer_14
#> 255 Biome_Inventory_layer_14
#> 256 Biome_Inventory_layer_14
#> 257 Biome_Inventory_layer_14
#> 258 Biome_Inventory_layer_14
#> 259 Biome_Inventory_layer_14
#> 260 Biome_Inventory_layer_14
#> 261 Biome_Inventory_layer_14
#> 262 Biome_Inventory_layer_14
#> 263 Biome_Inventory_layer_14
#> 264 Biome_Inventory_layer_14
#> 265 Biome_Inventory_layer_14
#> 266 Biome_Inventory_layer_14
#> 267 Biome_Inventory_layer_14
#> 268 Biome_Inventory_layer_14
#> 269 Biome_Inventory_layer_14
#> 270 Biome_Inventory_layer_14
#> 271 Biome_Inventory_layer_14
#> 272 Biome_Inventory_layer_15
#> 273 Biome_Inventory_layer_15
#> 274 Biome_Inventory_layer_15
#> 275 Biome_Inventory_layer_15
#> 276 Biome_Inventory_layer_15
#> 277 Biome_Inventory_layer_15
#> 278 Biome_Inventory_layer_15
#> 279 Biome_Inventory_layer_15
#> 280 Biome_Inventory_layer_15
#> 281 Biome_Inventory_layer_15
#> 282 Biome_Inventory_layer_15
#> 283 Biome_Inventory_layer_15
#> 284 Biome_Inventory_layer_15
#> 285 Biome_Inventory_layer_16
#> 286 Biome_Inventory_layer_16
#> 287 Biome_Inventory_layer_16
#> 288 Biome_Inventory_layer_16
#> 289 Biome_Inventory_layer_16
#> 290 Biome_Inventory_layer_16
#> 291 Biome_Inventory_layer_16
#> 292 Biome_Inventory_layer_16
#> 293 Biome_Inventory_layer_16
#> 294 Biome_Inventory_layer_16
#> 295 Biome_Inventory_layer_16
#> 296 Biome_Inventory_layer_16
#> 297 Biome_Inventory_layer_16
#> 298 Biome_Inventory_layer_16
#> 299 Biome_Inventory_layer_16
#> 300 Biome_Inventory_layer_16
#> 301 Biome_Inventory_layer_16
#> 302 Biome_Inventory_layer_16
#> 303 Biome_Inventory_layer_16
#> 304 Biome_Inventory_layer_16
#> 305 Biome_Inventory_layer_17
#> 306 Biome_Inventory_layer_17
#> 307 Biome_Inventory_layer_17
#> 308 Biome_Inventory_layer_17
#> 309 Biome_Inventory_layer_17
#> 310 Biome_Inventory_layer_17
#> 311 Biome_Inventory_layer_17
#> 312 Biome_Inventory_layer_17
#> 313 Biome_Inventory_layer_17
#> 314 Biome_Inventory_layer_17
#> 315 Biome_Inventory_layer_17
#> 316 Biome_Inventory_layer_17
#> 317 Biome_Inventory_layer_17
#> 318 Biome_Inventory_layer_17
#> 319 Biome_Inventory_layer_17
#> 320 Biome_Inventory_layer_17
#> 321 Biome_Inventory_layer_17
#> 322 Biome_Inventory_layer_17
#> 323 Biome_Inventory_layer_17
#> 324 Biome_Inventory_layer_17
#> 325 Biome_Inventory_layer_17
#> 326 Biome_Inventory_layer_17
#> 327 Biome_Inventory_layer_18
#> 328 Biome_Inventory_layer_18
#> 329 Biome_Inventory_layer_18
#> 330 Biome_Inventory_layer_18
#> 331 Biome_Inventory_layer_18
#> 332 Biome_Inventory_layer_18
#> 333 Biome_Inventory_layer_18
#> 334 Biome_Inventory_layer_18
#> 335 Biome_Inventory_layer_18
#> 336 Biome_Inventory_layer_18
#> 337 Biome_Inventory_layer_18
#> 338 Biome_Inventory_layer_18
#> 339 Biome_Inventory_layer_18
#> 340 Biome_Inventory_layer_18
#> 341 Biome_Inventory_layer_18
#> 342 Biome_Inventory_layer_18
#> 343 Biome_Inventory_layer_19
#> 344 Biome_Inventory_layer_19
#> 345 Biome_Inventory_layer_19
#> 346 Biome_Inventory_layer_19
#> 347 Biome_Inventory_layer_19
#> 348 Biome_Inventory_layer_19
#> 349 Biome_Inventory_layer_19
#> 350 Biome_Inventory_layer_19
#> 351 Biome_Inventory_layer_19
#> 352 Biome_Inventory_layer_19
#> 353 Biome_Inventory_layer_19
#> 354 Biome_Inventory_layer_19
#> 355 Biome_Inventory_layer_19
#> 356 Biome_Inventory_layer_19
#> 357 Biome_Inventory_layer_19
#> 358 Biome_Inventory_layer_19
#> 359 Biome_Inventory_layer_19
#> 360 Biome_Inventory_layer_20
#> 361 Biome_Inventory_layer_20
#> 362 Biome_Inventory_layer_20
#> 363 Biome_Inventory_layer_20
#> 364 Biome_Inventory_layer_20
#> 365 Biome_Inventory_layer_20
#> 366 Biome_Inventory_layer_20
#> 367 Biome_Inventory_layer_20
#> 368 Biome_Inventory_layer_20
#> 369 Biome_Inventory_layer_20
#> 370 Biome_Inventory_layer_20
#> 371 Biome_Inventory_layer_20
#> 372 Biome_Inventory_layer_20
#> 373 Biome_Inventory_layer_20
#> 374 Biome_Inventory_layer_20
#> 375 Biome_Inventory_layer_20
#> 376 Biome_Inventory_layer_20
#> 377 Biome_Inventory_layer_20
#> 378 Biome_Inventory_layer_20
#> 379 Biome_Inventory_layer_20
#> 380 Biome_Inventory_layer_20
#> 381 Biome_Inventory_layer_20
#> 382 Biome_Inventory_layer_20
#> 383 Biome_Inventory_layer_20
#> 384 Biome_Inventory_layer_20
#> 385 Biome_Inventory_layer_20
#> 386 Biome_Inventory_layer_20
#> 387 Biome_Inventory_layer_20
#> 388 Biome_Inventory_layer_21
#> 389 Biome_Inventory_layer_21
#> 390 Biome_Inventory_layer_21
#> 391 Biome_Inventory_layer_21
#> 392 Biome_Inventory_layer_21
#> 393 Biome_Inventory_layer_21
#> 394 Biome_Inventory_layer_21
#> 395 Biome_Inventory_layer_21
#> 396 Biome_Inventory_layer_21
#> 397 Biome_Inventory_layer_21
#> 398 Biome_Inventory_layer_21
#> 399 Biome_Inventory_layer_21
#> 400 Biome_Inventory_layer_21
#> 401 Biome_Inventory_layer_21
#> 402 Biome_Inventory_layer_21
#> 403 Biome_Inventory_layer_21
#> 404 Biome_Inventory_layer_21
#> 405 Biome_Inventory_layer_21
#> 406 Biome_Inventory_layer_21
#> 407 Biome_Inventory_layer_21
#> 408 Biome_Inventory_layer_21
#> 409 Biome_Inventory_layer_21
#> 410 Biome_Inventory_layer_22
#> 411 Biome_Inventory_layer_22
#> 412 Biome_Inventory_layer_22
#> 413 Biome_Inventory_layer_22
#> 414 Biome_Inventory_layer_22
#> 415 Biome_Inventory_layer_22
#> 416 Biome_Inventory_layer_22
#> 417 Biome_Inventory_layer_22
#> 418 Biome_Inventory_layer_22
#> 419 Biome_Inventory_layer_22
#> 420 Biome_Inventory_layer_22
#> 421 Biome_Inventory_layer_22
#> 422 Biome_Inventory_layer_22
#> 423 Biome_Inventory_layer_22
#> 424 Biome_Inventory_layer_22
#> 425 Biome_Inventory_layer_22
#> 426 Biome_Inventory_layer_22
#> 427 Biome_Inventory_layer_22
#> 428 Biome_Inventory_layer_22
#> 429 Biome_Inventory_layer_22
#> 430 Biome_Inventory_layer_22
#> 431 Biome_Inventory_layer_22
#> 432 Biome_Inventory_layer_22
#> 433 Biome_Inventory_layer_22
#> 434 Biome_Inventory_layer_22
#> 435 Biome_Inventory_layer_22
#> 436 Biome_Inventory_layer_22
#> 437 Biome_Inventory_layer_22
#> 438 Biome_Inventory_layer_23
#> 439 Biome_Inventory_layer_23
#> 440 Biome_Inventory_layer_23
#> 441 Biome_Inventory_layer_23
#> 442 Biome_Inventory_layer_23
#> 443 Biome_Inventory_layer_23
#> 444 Biome_Inventory_layer_23
#> 445 Biome_Inventory_layer_23
#> 446 Biome_Inventory_layer_23
#> 447 Biome_Inventory_layer_23
#> 448 Biome_Inventory_layer_23
#> 449 Biome_Inventory_layer_23
#> 450 Biome_Inventory_layer_23
#> 451 Biome_Inventory_layer_23
#> 452 Biome_Inventory_layer_23
#> 453 Biome_Inventory_layer_23
#> 454 Biome_Inventory_layer_23
#> 455 Biome_Inventory_layer_24
#> 456 Biome_Inventory_layer_24
#> 457 Biome_Inventory_layer_24
#> 458 Biome_Inventory_layer_24
#> 459 Biome_Inventory_layer_24
#> 460 Biome_Inventory_layer_24
#> 461 Biome_Inventory_layer_24
#> 462 Biome_Inventory_layer_24
#> 463 Biome_Inventory_layer_24
#> 464 Biome_Inventory_layer_24
#> 465 Biome_Inventory_layer_24
#> 466 Biome_Inventory_layer_24
#> 467 Biome_Inventory_layer_24
#> 468 Biome_Inventory_layer_24
#> 469 Biome_Inventory_layer_24
#> 470 Biome_Inventory_layer_24
#> 471 Biome_Inventory_layer_24
#> 472 Biome_Inventory_layer_25
#> 473 Biome_Inventory_layer_25
#> 474 Biome_Inventory_layer_25
#> 475 Biome_Inventory_layer_25
#> 476 Biome_Inventory_layer_25
#> 477 Biome_Inventory_layer_25
#> 478 Biome_Inventory_layer_25
#> 479 Biome_Inventory_layer_25
#> 480 Biome_Inventory_layer_25
#> 481 Biome_Inventory_layer_25
#> 482 Biome_Inventory_layer_25
#> 483 Biome_Inventory_layer_25
#> 484 Biome_Inventory_layer_25
#> 485 Biome_Inventory_layer_26
#> 486 Biome_Inventory_layer_26
#> 487 Biome_Inventory_layer_26
#> 488 Biome_Inventory_layer_26
#> 489 Biome_Inventory_layer_26
#> 490 Biome_Inventory_layer_26
#> 491 Biome_Inventory_layer_26
#> 492 Biome_Inventory_layer_26
#> 493 Biome_Inventory_layer_26
#> 494 Biome_Inventory_layer_26
#> 495 Biome_Inventory_layer_26
#> 496 Biome_Inventory_layer_26
#> 497 Biome_Inventory_layer_26
#> 498 Biome_Inventory_layer_26
#> 499 Biome_Inventory_layer_26
#> 500 Biome_Inventory_layer_26
#> 501 Biome_Inventory_layer_26
#> 502 Biome_Inventory_layer_26
#> 503 Biome_Inventory_layer_26
#> 504 Biome_Inventory_layer_26
#> 505 Biome_Inventory_layer_26
#> 506 Biome_Inventory_layer_26
#> 507 Biome_Inventory_layer_26
#> 508 Biome_Inventory_layer_26
#> 509 Biome_Inventory_layer_26
#> 510 Biome_Inventory_layer_26
#> 511 Biome_Inventory_layer_26
#> 512 Biome_Inventory_layer_26
#> 513 Biome_Inventory_layer_26
#> 514 Biome_Inventory_layer_26
#> 515 Biome_Inventory_layer_26
#> 516 Biome_Inventory_layer_26
#> 517 Biome_Inventory_layer_26
#> 518 Biome_Inventory_layer_26
#> 519 Biome_Inventory_layer_26
#> 520 Biome_Inventory_layer_26
#> 521 Biome_Inventory_layer_26
#> 522 Biome_Inventory_layer_26
#> 523 Biome_Inventory_layer_27
#> 524 Biome_Inventory_layer_27
#> 525 Biome_Inventory_layer_27
#> 526 Biome_Inventory_layer_27
#> 527 Biome_Inventory_layer_27
#> 528 Biome_Inventory_layer_27
#> 529 Biome_Inventory_layer_27
#> 530 Biome_Inventory_layer_27
#> 531 Biome_Inventory_layer_27
#> 532 Biome_Inventory_layer_27
#> 533 Biome_Inventory_layer_27
#> 534 Biome_Inventory_layer_27
#> 535 Biome_Inventory_layer_27
#> 536 Biome_Inventory_layer_27
#> 537 Biome_Inventory_layer_27
#> 538 Biome_Inventory_layer_27
#> 539 Biome_Inventory_layer_27
#> 540 Biome_Inventory_layer_28
#> 541 Biome_Inventory_layer_28
#> 542 Biome_Inventory_layer_28
#> 543 Biome_Inventory_layer_28
#> 544 Biome_Inventory_layer_28
#> 545 Biome_Inventory_layer_28
#> 546 Biome_Inventory_layer_28
#> 547 Biome_Inventory_layer_28
#> 548 Biome_Inventory_layer_28
#> 549 Biome_Inventory_layer_28
#> 550 Biome_Inventory_layer_28
#> 551 Biome_Inventory_layer_28
#> 552 Biome_Inventory_layer_28
#> 553 Biome_Inventory_layer_28
#> 554 Biome_Inventory_layer_28
#> 555 Biome_Inventory_layer_28
#> 556 Biome_Inventory_layer_29
#> 557 Biome_Inventory_layer_29
#> 558 Biome_Inventory_layer_29
#> 559 Biome_Inventory_layer_29
#> 560 Biome_Inventory_layer_29
#> 561 Biome_Inventory_layer_29
#> 562 Biome_Inventory_layer_29
#> 563 Biome_Inventory_layer_29
#> 564 Biome_Inventory_layer_29
#> 565 Biome_Inventory_layer_29
#> 566 Biome_Inventory_layer_29
#> 567 Biome_Inventory_layer_29
#> 568 Biome_Inventory_layer_29
#> 569 Biome_Inventory_layer_29
#> 570 Biome_Inventory_layer_29
#> 571 Biome_Inventory_layer_29
#> 572 Biome_Inventory_layer_29
#> 573 Biome_Inventory_layer_29
#> 574 Biome_Inventory_layer_29
#> 575 Biome_Inventory_layer_29
#> 576 Biome_Inventory_layer_29
#> 577 Biome_Inventory_layer_29
#> 578 Biome_Inventory_layer_29
#> 579 Biome_Inventory_layer_29
#> 580 Biome_Inventory_layer_29
#> 581 Biome_Inventory_layer_29
#> 582 Biome_Inventory_layer_29
#> 583 Biome_Inventory_layer_29
#> 584 Biome_Inventory_layer_29
#> 585 Biome_Inventory_layer_29
#> 586 Biome_Inventory_layer_29
#> 587 Biome_Inventory_layer_30
#> 588 Biome_Inventory_layer_30
#> 589 Biome_Inventory_layer_30
#> 590 Biome_Inventory_layer_30
#> 591 Biome_Inventory_layer_30
#> 592 Biome_Inventory_layer_30
#> 593 Biome_Inventory_layer_30
#> 594 Biome_Inventory_layer_30
#> 595 Biome_Inventory_layer_30
#> 596 Biome_Inventory_layer_30
#> 597 Biome_Inventory_layer_30
#> 598 Biome_Inventory_layer_30
#> 599 Biome_Inventory_layer_30
#> 600 Biome_Inventory_layer_30
#> 601 Biome_Inventory_layer_31
#> 602 Biome_Inventory_layer_31
#> 603 Biome_Inventory_layer_31
#> 604 Biome_Inventory_layer_31
#> 605 Biome_Inventory_layer_31
#> 606 Biome_Inventory_layer_31
#> 607 Biome_Inventory_layer_31
#> 608 Biome_Inventory_layer_31
#> 609 Biome_Inventory_layer_31
#> 610 Biome_Inventory_layer_31
#> 611 Biome_Inventory_layer_31
#> 612 Biome_Inventory_layer_31
#> 613 Biome_Inventory_layer_31
#> 614 Biome_Inventory_layer_31
#> 615 Biome_Inventory_layer_31
#> 616 Biome_Inventory_layer_31
#> 617 Biome_Inventory_layer_31
#> 618 Biome_Inventory_layer_31
#> 619 Biome_Inventory_layer_31
#> 620 Biome_Inventory_layer_31
#> 621 Biome_Inventory_layer_31
#> 622 Biome_Inventory_layer_31
#> 623 Biome_Inventory_layer_31
#> 624 Biome_Inventory_layer_31
#> 625 Biome_Inventory_layer_31
#> 626 Biome_Inventory_layer_31
#> 627 Biome_Inventory_layer_31
#> 628 Biome_Inventory_layer_31
#> 629 Biome_Inventory_layer_31
#> 630 Biome_Inventory_layer_31
#> 631 Biome_Inventory_layer_31
#> 632 Biome_Inventory_layer_31
#> 633 Biome_Inventory_layer_31
#> 634 Biome_Inventory_layer_31
#>                                                                                                   biome
#> 1                                                                    Boreal evergreen needleleaf forest
#> 2                                                                                       Boreal parkland
#> 3                                                                   Boreal summergreen broadleaf forest
#> 4                                                                                                Desert
#> 5                                                                                               Savanna
#> 6                                                                                            Semidesert
#> 7                                                                                          Shrub tundra
#> 8                                                                                                Steppe
#> 9                                                                  Temperate broadleaf evergreen forest
#> 10                                                                               Temperate mixed forest
#> 11                                                                Temperate needleleaf evergreen forest
#> 12                                                                                   Temperate parkland
#> 13                                                                                  Temperate shrubland
#> 14                                                                         Temperate summergreen forest
#> 15                                                                            Tropical evergreen forest
#> 16                                                                                   Tropical grassland
#> 17                                                                            Tropical raingreen forest
#> 18                                                                                               Tundra
#> 19                                                                              Warm temperate woodland
#> 20                                                                                             no_biome
#> 21                                                                          Bare soil/sparse vegetation
#> 22                                                                  Closed forest (deciduous broadleaf)
#> 23                                                                 Closed forest (deciduous needleleaf)
#> 24                                                                  Closed forest (evergreen broadleaf)
#> 25                                                                 Closed forest (evergreen needleleaf)
#> 26                                                                                Closed forest (mixed)
#> 27                                                                              Closed forest (unknown)
#> 28                                             Cultivated and managed vegetation/agriculture (cropland)
#> 29                                                                                Herbaceous vegetation
#> 30                                                                                   Herbaceous wetland
#> 31                                                                                         Inland water
#> 32                                                                    Open forest (deciduous broadleaf)
#> 33                                                                   Open forest (evergreen needleleaf)
#> 34                                                                                Open forest (unknown)
#> 35                                                                                               Shrubs
#> 36                                                                                         Snow and ice
#> 37                                                                                                Urban
#> 38                                                                                             no_biome
#> 39                                                                             Af - Tropical rainforest
#> 40                                                                                Am - Tropical monsoon
#> 41                                                                                Aw - Tropical savanna
#> 42                                                                                BSh - Arid steppe hot
#> 43                                                                               BSk - Arid steppe cold
#> 44                                                                                BWh - Arid desert hot
#> 45                                                                               BWk - Arid desert cold
#> 46                                                             Cfa - Temperate no dry season hot summer
#> 47                                                            Cfb - Temperate no dry season warm summer
#> 48                                                            Cfc - Temperate no dry season cold summer
#> 49                                                                Csa - Temperate dry summer hot summer
#> 50                                                               Csb - Temperate dry summer warm summer
#> 51                                                                Cwa - Temperate dry winter hot summer
#> 52                                                               Cwb - Temperate dry winter warm summer
#> 53                                                                  Dfa - Cold no dry season hot summer
#> 54                                                                 Dfb - Cold no dry season warm summer
#> 55                                                                 Dfc - Cold no dry season cold summer
#> 56                                                                     Dsa - Cold dry summer hot summer
#> 57                                                                    Dsb - Cold dry summer warm summer
#> 58                                                                    Dsc - Cold dry summer cold summer
#> 59                                                                     Dwa - Cold dry winter hot summer
#> 60                                                                    Dwb - Cold dry winter warm summer
#> 61                                                                    Dwc - Cold dry winter cold summer
#> 62                                                                                     EF - Polar frost
#> 63                                                                                    ET - Polar tundra
#> 64                                                                                             no_biome
#> 65                                                                                Cold deciduous forest
#> 66                                                                     Cold evergreen needleleaf forest
#> 67                                                                     Cool evergreen needleleaf forest
#> 68                                                                                    Cool mixed forest
#> 69                                                                            Cool temperate rainforest
#> 70                                                                                               Desert
#> 71                                                                             Erect dwarf shrub tundra
#> 72                                                                            Graminoid and forb tundra
#> 73                                                                            Low and high shrub tundra
#> 74                                                                                               Steppe
#> 75                                                                 Temperate deciduous broadleaf forest
#> 76                                                         Temperate evergreen needleleaf open woodland
#> 77                                                         Temperate sclerophyll woodland and shrubland
#> 78                                                     Tropical deciduous broadleaf forest and woodland
#> 79                                                                  Tropical evergreen broadleaf forest
#> 80                                                                                     Tropical savanna
#> 81                                                             Tropical semi-evergreen broadleaf forest
#> 82                                                            Warm temperate evergreen and mixed forest
#> 83                                                                            Xerophytic woodland scrub
#> 84                                                                                             no_biome
#> 85                                                                                  Boreal forest/taiga
#> 86                                                                          Deserts and xeric shrubland
#> 87                                                                        Flooded grassland and savanna
#> 88                                                                                             Mangrove
#> 89                                                             Mediterranean forest woodland and scrub 
#> 90                                                                      Montane grassland and shrubland
#> 91                                                                                         Rock and ice
#> 92                                                                 Temperate broadleaf and mixed forest
#> 93                                                                             Temperate conifer forest
#> 94                                                            Temperate grassland savanna and shrubland
#> 95                                                           Tropical and subtropical coniferous forest
#> 96                                                        Tropical and subtropical dry broadleaf forest
#> 97                                             Tropical and subtropical grassland savanna and shrubland
#> 98                                                      Tropical and subtropical moist broadleaf forest
#> 99                                                                                               Tundra
#> 100                                                                                            no_biome
#> 101                                                                  Frigid deciduous coniferous forest
#> 102                                                                  Frigid evergreen coniferous forest
#> 103                                                                                         Polar frost
#> 104                                                                                        Polar tundra
#> 105                                                                             Sub-frigid mixed forest
#> 106                                                 Temperate continental climate with deciduous forest
#> 107                                                                                    Temperate desert
#> 108                                                                                 Temperate grassland
#> 109                                          Temperate maritime climate with evergreen broadleaf forest
#> 110                                                               Tropical Sahel and semiarid grassland
#> 111                                                                                     Tropical desert
#> 112                                                                                     Tropical forest
#> 113                                                                                  Tropical grassland
#> 114                                                                             Tropical monsoon forest
#> 115                                                                                            no_biome
#> 116                                                                                          Cluster 10
#> 117                                                                                          Cluster 11
#> 118                                                                                          Cluster 12
#> 119                                                                                          Cluster 13
#> 120                                                                                           Cluster 2
#> 121                                                                                           Cluster 3
#> 122                                                                                           Cluster 4
#> 123                                                                                           Cluster 5
#> 124                                                                                           Cluster 6
#> 125                                                                                           Cluster 7
#> 126                                                                                           Cluster 8
#> 127                                                                                           Cluster 9
#> 128                                                                                            no_biome
#> 129                                                                                           Cluster 1
#> 130                                                                                          Cluster 10
#> 131                                                                                          Cluster 11
#> 132                                                                                          Cluster 12
#> 133                                                                                          Cluster 13
#> 134                                                                                           Cluster 2
#> 135                                                                                           Cluster 3
#> 136                                                                                           Cluster 4
#> 137                                                                                           Cluster 5
#> 138                                                                                           Cluster 6
#> 139                                                                                           Cluster 7
#> 140                                                                                           Cluster 8
#> 141                                                                                           Cluster 9
#> 142                                                                                            no_biome
#> 143                                                                                                 SHB
#> 144                                                                                                 SHC
#> 145                                                                                                 SHD
#> 146                                                                                                 SHN
#> 147                                                                                                 SLB
#> 148                                                                                                 SLC
#> 149                                                                                                 SLD
#> 150                                                                                                 SLN
#> 151                                                                                                 SMB
#> 152                                                                                                 SMC
#> 153                                                                                                 SMD
#> 154                                                                                                 SMN
#> 155                                                                                                 THB
#> 156                                                                                                 THC
#> 157                                                                                                 THD
#> 158                                                                                                 THN
#> 159                                                                                                 TLB
#> 160                                                                                                 TLC
#> 161                                                                                                 TLD
#> 162                                                                                                 TLN
#> 163                                                                                                 TMB
#> 164                                                                                                 TMC
#> 165                                                                                                 TMD
#> 166                                                                                                 TMN
#> 167                                                                                            no_biome
#> 168                                                  Evergreen and seasonal tropical lowland rainforest
#> 169                                                 Evergreen and summergreen forest, tall-grass steppe
#> 170                                                                  Evergreen boreal coniferous forest
#> 171                                                                               Evergreen dry savanna
#> 172                                                                             Evergreen moist savanna
#> 173                                                                 Evergreen nemoral Nothofagus forest
#> 174                                                                 Evergreen nemoral coniferous forest
#> 175                                                                     Evergreen nemoral laural forest
#> 176                                                                 Evergreen subtropical laurel forest
#> 177                                                        Hemiboreal deciduous coniferous mixed forest
#> 178                                                                     High mountian steppe semidesert
#> 179                                                                                        Inland water
#> 180                                                                          Mixed and low-grass steppe
#> 181                                                                                           Mountains
#> 182                                                                                      Nemoral desert
#> 183                                                                                  Nemoral dry forest
#> 184                                                                       Nemoral dwarf bush semidesert
#> 185                                                                                     Oceanic islands
#> 186                                                                   Polar gras and dwarf shrub tundra
#> 187                                                                               Raingreen dry savanna
#> 188                                                                             Raingreen moist savanna
#> 189                                                        Semievergreen and raingreen deciduous forest
#> 190                                                                               Subtropical grassland
#> 191                                                                      Subtropical sclerophyll forest
#> 192                                                                 Summergreen boreal deciduous forest
#> 193                                                                Summergreen nemoral deciduous forest
#> 194                                                                         Tropical-subtropical desert
#> 195                                                                     Tropical-subtropical dry forest
#> 196                                                          Tropical-subtropical dwarf bush semidesert
#> 197                                                               Tropical-subtropical grass semidesert
#> 198                                                           Tropical-subtropical succulent semidesert
#> 199                                                                                            no_biome
#> 200                                                                  Frigid deciduous coniferous forest
#> 201                                                                                        Polar tundra
#> 202                                                                             Sub-frigid mixed forest
#> 203                                                 Temperate continental climate with deciduous forest
#> 204                                                                                    Temperate desert
#> 205                                                                                 Temperate grassland
#> 206                                          Temperate maritime climate with evergreen broadleaf forest
#> 207                                                               Tropical Sahel and semiarid grassland
#> 208                                                                                     Tropical desert
#> 209                                                                                     Tropical forest
#> 210                                                                                  Tropical grassland
#> 211                                                                             Tropical monsoon forest
#> 212                                                                                            no_biome
#> 213                                                                                              Arctic
#> 214                                                                                      Cold and mesic
#> 215                                                                                        Cold and wet
#> 216                                                                              Cool temperate and dry
#> 217                                                                            Cool temperate and moist
#> 218                                                                            Cool temperate and xeric
#> 219                                                                            Extremely cold and mesic
#> 220                                                                              Extremely cold and wet
#> 221                                                                              Extremely hot and arid
#> 222                                                                             Extremely hot and moist
#> 223                                                                             Extremely hot and xeric
#> 224                                                                                        Hot and arid
#> 225                                                                                         Hot and dry
#> 226                                                                                       Hot and mesic
#> 227                                                                            Warm temperate and mesic
#> 228                                                                            Warm temperate and xeric
#> 229                                                                                            no_biome
#> 230                                                                            Boreal coniferous forest
#> 231                                                                              Boreal mountain system
#> 232                                                                              Boreal tundra woodland
#> 233                                                                                        Inland water
#> 234                                                                                               Polar
#> 235                                                                                  Subtropical desert
#> 236                                                                              Subtropical dry forest
#> 237                                                                            Subtropical humid forest
#> 238                                                                         Subtropical mountain system
#> 239                                                                                  Subtropical steppe
#> 240                                                                        Temperate continental forest
#> 241                                                                                    Temperate desert
#> 242                                                                           Temperate mountain system
#> 243                                                                            Temperate oceanic forest
#> 244                                                                                    Temperate steppe
#> 245                                                                                     Tropical desert
#> 246                                                                                 Tropical dry forest
#> 247                                                                               Tropical moist forest
#> 248                                                                            Tropical mountain system
#> 249                                                                                 Tropical rainforest
#> 250                                                                                  Tropical shrubland
#> 251                                                                                            no_biome
#> 252                                                          Bare soil - consolidated (gravel and rock)
#> 253                                                                   Bare soil - unconsolidated (sand)
#> 254                                                                          Broadleaf deciduous forest
#> 255                                                                          Broadleaf evergreen forest
#> 256                                                                                            Cropland
#> 257                                                                    Cropland/other vegetation mosaic
#> 258                                                                                          Herbaceous
#> 259                                                                   Herbaceous with sparse tree/shrub
#> 260                                                                                            Mangrove
#> 261                                                                                        Mixed forest
#> 262                                                                         Needleleaf deciduous forest
#> 263                                                                         Needleleaf evergreen forest
#> 264                                                                                         Paddy field
#> 265                                                                                               Shrub
#> 266                                                                                        Snow and ice
#> 267                                                                                   Sparse vegetation
#> 268                                                                                           Tree open
#> 269                                                                                               Urban
#> 270                                                                                             Wetland
#> 271                                                                                            no_biome
#> 272                                                                                              Barren
#> 273                                                                           Closed bushland/shrubland
#> 274                                                                                            Cropland
#> 275                                                                          Deciduous broadleaf forest
#> 276                                                                          Evergreen broadleaf forest
#> 277                                                                         Evergreen needleleaf forest
#> 278                                                                                           Grassland
#> 279                                                                                        Mixed forest
#> 280                                                                                      Open shrubland
#> 281                                                                                               Urban
#> 282                                                                          Wooded grassland shrubland
#> 283                                                                                            Woodland
#> 284                                                                                            no_biome
#> 285                                                                  Inhabited treeless and barren land
#> 286                                                                                  Irrigated villages
#> 287                                                                                   Mixed settlements
#> 288                                                                                   Pastoral villages
#> 289                                                                                  Populated cropland
#> 290                                                                                 Populated rangeland
#> 291                                                                                  Populated woodland
#> 292                                                                                    Rainfed villages
#> 293                                                                                     Remote cropland
#> 294                                                                                    Remote rangeland
#> 295                                                                                     Remote woodland
#> 296                                                                      Residential irrigated cropland
#> 297                                                                        Residential rainfed cropland
#> 298                                                                               Residential rangeland
#> 299                                                                                Residential woodland
#> 300                                                                                       Rice villages
#> 301                                                                                               Urban
#> 302                                                                       Wild treeless and barren land
#> 303                                                                                       Wild woodland
#> 304                                                                                            no_biome
#> 305                                                                                           Bare soil
#> 306                                                      Closed (>40%) broadleaf deciduous forest (>5m)
#> 307             Closed (>40%) broadleaf forest or shrubland permanently flooded (saline/brackish water)
#> 308                                                     Closed (>40%) needleleaf evergreen forest (>5m)
#> 309              Closed to open (>15%) (broadleaf or needleleaf evergreen or deciduous) shrubland (<5m)
#> 310                            Closed to open (>15%) broadleaf evergreen or semi-deciduous forest (>5m)
#> 311                     Closed to open (>15%) broadleaf forest regularly flooded (fresh/brackish water)
#> 312 Closed to open (>15%) grassland or woody vegetation regularly flooded (fresh/brackish/saline water)
#> 313                      Closed to open (>15%) herbaceous vegetation (grassland savanna or lichen/moss)
#> 314                                   Closed to open (>15%) mixed broadleaf and needleleaf forest (>5m)
#> 315                           Mosaic cropland (50-70%)/vegetation (grassland/shrubland/forest) (20-50%)
#> 316                                              Mosaic forest or shrubland (50-70%)/grassland (20-50%)
#> 317                                              Mosaic grassland (50-70%)/forest or shrubland (20-50%)
#> 318                           Mosaic vegetation (grassland/shrubland/forest) (50-70%)/cropland (20-50%)
#> 319                                             Open (15-40%) broadleaf deciduous forest/woodland (>5m)
#> 320                                        Open (15-40%) needleleaf deciduous or evergreen forest (>5m)
#> 321                                                    Post-flooding or irrigated cropland (or aquatic)
#> 322                                                                                    Rainfed cropland
#> 323                                                                                        Snow and ice
#> 324                                                                            Sparse (<15%) vegetation
#> 325                                                                                               Urban
#> 326                                                                                            no_biome
#> 327                                                                                              Barren
#> 328                                                                                    Closed shrubland
#> 329                                                                                            Cropland
#> 330                                                                  Cropland/natural vegetation mosaic
#> 331                                                                          Deciduous broadleaf forest
#> 332                                                                          Evergreen broadleaf forest
#> 333                                                                         Evergreen needleleaf forest
#> 334                                                                                           Grassland
#> 335                                                                                        Mixed forest
#> 336                                                                                      Open shrubland
#> 337                                                                                   Permanent wetland
#> 338                                                                                             Savanna
#> 339                                                                                        Snow and ice
#> 340                                                                                               Urban
#> 341                                                                                       Woody savanna
#> 342                                                                                            no_biome
#> 343                                                                                 Boreal forest/taiga
#> 344                                                                              Desert and xeric shrub
#> 345                                                                       Flooded grassland and savanna
#> 346                                                                                        Inland water
#> 347                                                                                            Mangrove
#> 348                                                             Mediterranean forest woodland and scrub
#> 349                                                                         Montane grassland and shrub
#> 350                                                                                        Rock and ice
#> 351                                                                Temperate broadleaf and mixed forest
#> 352                                                                            Temperate conifer forest
#> 353                                                               Temperate grassland savanna and shrub
#> 354                                                              Tropical subtropical coniferous forest
#> 355                                                           Tropical subtropical dry broadleaf forest
#> 356                                                    Tropical subtropical grassland savanna and shrub
#> 357                                                         Tropical subtropical moist broadleaf forest
#> 358                                                                                              Tundra
#> 359                                                                                            no_biome
#> 360                                                                            Af - Tropical rainforest
#> 361                                                                               Am - Tropical monsoon
#> 362                                                                               Aw - Tropical savanna
#> 363                                                                               BSh - Arid steppe hot
#> 364                                                                              BSk - Arid steppe cold
#> 365                                                                               BWh - Arid desert hot
#> 366                                                                              BWk - Arid desert cold
#> 367                                                            Cfa - Temperate no dry season hot summer
#> 368                                                           Cfb - Temperate no dry season warm summer
#> 369                                                           Cfc - Temperate no dry season cold summer
#> 370                                                               Csa - Temperate dry summer hot summer
#> 371                                                              Csb - Temperate dry summer warm summer
#> 372                                                               Cwa - Temperate dry winter hot summer
#> 373                                                              Cwb - Temperate dry winter warm summer
#> 374                                                                 Dfa - Cold no dry season hot summer
#> 375                                                                Dfb - Cold no dry season warm summer
#> 376                                                                Dfc - Cold no dry season cold summer
#> 377                                                                    Dsa - Cold dry summer hot summer
#> 378                                                                   Dsb - Cold dry summer warm summer
#> 379                                                                   Dsc - Cold dry summer cold summer
#> 380                                                                    Dwa - Cold dry winter hot summer
#> 381                                                                   Dwb - Cold dry winter warm summer
#> 382                                                                   Dwc - Cold dry winter cold summer
#> 383                                                                                    EF - Polar frost
#> 384                                                                          EFH - Polar frost highland
#> 385                                                                                   ET - Polar tundra
#> 386                                                                         ETH - Polar tundra highland
#> 387                                                                                            no_biome
#> 388                                                                                           Bare soil
#> 389                                                                         Cultivated and managed area
#> 390                                                                      Herbaceous cover (closed-open)
#> 391                                                                         Mosaic cropland/shrub/grass
#> 392                                                 Mosaic cropland/tree cover/other natural vegetation
#> 393                                                          Mosaic tree cover/other natural vegetation
#> 394                                                            Regularly flooded shrub/herbaceous cover
#> 395                                                                 Shrub cover (closed-open deciduous)
#> 396                                                                 Shrub cover (closed-open evergreen)
#> 397                                                                                        Snow and ice
#> 398                                                                       Sparse herbaceous/shrub cover
#> 399                                                             Tree cover (broadleaf deciduous closed)
#> 400                                                               Tree cover (broadleaf deciduous open)
#> 401                                                                    Tree cover (broadleaf evergreen)
#> 402                                                                                  Tree cover (burnt)
#> 403                                                                        Tree cover (mixed leaf type)
#> 404                                                                   Tree cover (needleleaf deciduous)
#> 405                                                                   Tree cover (needleleaf evergreen)
#> 406                                                          Tree cover (regularly flooded fresh water)
#> 407                                                         Tree cover (regularly flooded saline water)
#> 408                                                                                               Urban
#> 409                                                                                            no_biome
#> 410                                                                                              Barren
#> 411                                                                                   Cold mixed forest
#> 412                                                                                 Cool conifer forest
#> 413                                                                                   Cool mixed forest
#> 414                                                                       Cushion-forbs lichen and moss
#> 415                                                                      Deciduous taiga/montane forest
#> 416                                                                                              Desert
#> 417                                                                                  Dwarf shrub tundra
#> 418                                                                       Evegreen taiga/montane forest
#> 419                                                                               Open conifer woodland
#> 420                                                                              Prostrate shrub tundra
#> 421                                                                                        Shrub tundra
#> 422                                                                                        Snow and ice
#> 423                                                                                       Steppe tundra
#> 424                                                                         Temperate broadleaf savanna
#> 425                                                                            Temperate conifer forest
#> 426                                                                          Temperate deciduous forest
#> 427                                                                                 Temperate grassland
#> 428                                                                      Temperate sclerophyll woodland
#> 429                                                                      Temperate xerophytic shrubland
#> 430                                                                  Tropical deciduous forest/woodland
#> 431                                                                           Tropical evergreen forest
#> 432                                                                                  Tropical grassland
#> 433                                                                                    Tropical savanna
#> 434                                                                      Tropical semi-deciduous forest
#> 435                                                                       Tropical xerophytic shrubland
#> 436                                                                                   Warm mixed forest
#> 437                                                                                            no_biome
#> 438                                                                                 Boreal forest/taiga
#> 439                                                                         Deserts and xeric shrubland
#> 440                                                                       Flooded grassland and savanna
#> 441                                                                                        Inland water
#> 442                                                                                            Mangrove
#> 443                                                             Mediterranean forest woodland and scrub
#> 444                                                                     Montane grassland and shrubland
#> 445                                                                                        Snow and ice
#> 446                                                                Temperate broadleaf and mixed forest
#> 447                                                                            Temperate conifer forest
#> 448                                                           Temperate grassland savanna and shrubland
#> 449                                                          Tropical and subtropical coniferous forest
#> 450                                                       Tropical and subtropical dry broadleaf forest
#> 451                                            Tropical and subtropical grassland savanna and shrubland
#> 452                                                     Tropical and subtropical moist broadleaf forest
#> 453                                                                                              Tundra
#> 454                                                                                            no_biome
#> 455                                                                                        Bare surface
#> 456                                                                                        Closed shrub
#> 457                                                                             Crop/natural vegetation
#> 458                                                                                            Cropland
#> 459                                                                                 Deciduous broadleaf
#> 460                                                                                Deciduous needleleaf
#> 461                                                                                 Evergreen broadleaf
#> 462                                                                                Evergreen needleleaf
#> 463                                                                                           Grassland
#> 464                                                                                                 Ice
#> 465                                                                                        Mixed forest
#> 466                                                                                          Open shrub
#> 467                                                                                             Savanna
#> 468                                                                                               Urban
#> 469                                                                                            Wetlands
#> 470                                                                                       Woody savanna
#> 471                                                                                            no_biome
#> 472                                                                                     Boreal woodland
#> 473                                                                                     Dense shrubland
#> 474                                                                                   Desert and barren
#> 475                                                                                Grassland and steppe
#> 476                                                                                      Mixed woodland
#> 477                                                                                      Open shrubland
#> 478                                                                                             Savanna
#> 479                                                                        Temperate deciduous woodland
#> 480                                                                        Temperate evergreen woodland
#> 481                                                                         Tropical deciduous woodland
#> 482                                                                         Tropical evergreen woodland
#> 483                                                                                              Tundra
#> 484                                                                                            no_biome
#> 485                                                                                       Boreal desert
#> 486                                                                                     Boreal dry bush
#> 487                                                                                 Boreal moist forest
#> 488                                                                                   Boreal rainforest
#> 489                                                                                   Boreal wet forest
#> 490                                                                               Cool temperate desert
#> 491                                                                          Cool temperate desert bush
#> 492                                                                         Cool temperate moist forest
#> 493                                                                           Cool temperate rainforest
#> 494                                                                               Cool temperate steppe
#> 495                                                                           Cool temperate wet forest
#> 496                                                                                                 Ice
#> 497                                                                                        Inland water
#> 498                                                                                        Polar desert
#> 499                                                                                    Polar dry tundra
#> 500                                                                                  Polar moist tundra
#> 501                                                                                   Polar rain tundra
#> 502                                                                                    Polar wet tundra
#> 503                                                                                  Subtropical desert
#> 504                                                                             Subtropical desert bush
#> 505                                                                              Subtropical dry forest
#> 506                                                                            Subtropical moist forest
#> 507                                                                              Subtropical rainforest
#> 508                                                                            Subtropical thorn steppe
#> 509                                                                              Subtropical wet forest
#> 510                                                                                     Tropical desert
#> 511                                                                                 Tropical dry forest
#> 512                                                                               Tropical moist forest
#> 513                                                                               Tropical thorn steppe
#> 514                                                                            Tropical very dry forest
#> 515                                                                                 Tropical wet forest
#> 516                                                                               Warm temperate desert
#> 517                                                                          Warm temperate desert bush
#> 518                                                                           Warm temperate dry forest
#> 519                                                                         Warm temperate moist forest
#> 520                                                                         Warm temperate thorn steppe
#> 521                                                                           Warm temperate wet forest
#> 522                                                                                            no_biome
#> 523                                                                                              Boreal
#> 524                                                                         Continuous moist subtropics
#> 525                                                                            Continuous moist tropics
#> 526                                                                                         Dry savanna
#> 527                                                                                        Inland water
#> 528                                                                                 Moist mid-latitudes
#> 529                                                                                       Moist savanna
#> 530                                                                                           Mountains
#> 531                                                                                     Oceanic islands
#> 532                                                                        Summer moist xeric shrubland
#> 533                                                                                    Temperate desert
#> 534                                                                                    Temperate steppe
#> 535                                                                         Tropical-subtropical desert
#> 536                                                                                              Tundra
#> 537                                                                                 Winter moist steppe
#> 538                                                                             Winter moist subtropics
#> 539                                                                                            no_biome
#> 540                                                                               Desert and semidesert
#> 541                                                                 Dry savanna and tropical dry forest
#> 542                                                                                          Dry steppe
#> 543                                                                                      Dry zone oasis
#> 544                                                                                        Inland water
#> 545                                                                                       Moist savanna
#> 546                                                                                     Oceanic islands
#> 547                                                                                        Rock and ice
#> 548                                                                           Temperate forest and bush
#> 549                                                                     Temperate grassland and pasture
#> 550                                                                                             Tillage
#> 551                                                                                Tropical agriculture
#> 552                                                                           Tropical pasture highland
#> 553                                                                                 Tropical rainforest
#> 554                                                                                              Tundra
#> 555                                                                                            no_biome
#> 556                                                                                Coniferous dry shrub
#> 557                                                                                          Dry desert
#> 558                                                                                         Dry savanna
#> 559                                                              Dry steppe and hard cushion formations
#> 560                                                                  Evergreen boreal coniferous forest
#> 561                                                                                        Inland water
#> 562                                                            Laurel forest and subtropical rainforest
#> 563                                                                                       Moist savanna
#> 564                                                                          Mountain coniferous forest
#> 565                                                                                 Mountain vegetation
#> 566                                                                                     Oceanic islands
#> 567                                                                           Paramo heath and wet Puna
#> 568                                                                              Sclerophyll vegetation
#> 569                                                                                          Semidesert
#> 570                                                                                  Subantarctic heath
#> 571                                                              Subpolar meadow and summer green shrub
#> 572                                                                      Summer green coniferous forest
#> 573                                                                       Summer green deciduous forest
#> 574                                                         Summer green deciduous forest with conifers
#> 575                                                                            Summer green tree steppe
#> 576                                                                                Temperate rainforest
#> 577                                                                          Thorn and succulent forest
#> 578                                                                                       Thorn savanna
#> 579                                                                          Thorn shrub and succulents
#> 580                                                                                   Transition steppe
#> 581                                                                                 Tropical dry forest
#> 582                                                                        Tropical mountain rainforest
#> 583                                                                                 Tropical rainforest
#> 584                                                                  Tropical semi-evergreen rainforest
#> 585                                                                                              Tundra
#> 586                                                                                            no_biome
#> 587                                                                                       Boreal forest
#> 588                                                                               Desert and semidesert
#> 589                                                                                        Inland water
#> 590                                                                                       Mediterranean
#> 591                                                                                     Oceanic islands
#> 592                                                                                    Temperate forest
#> 593                                                                                 Temperate grassland
#> 594                                                                                 Tropical rainforest
#> 595                                                                                    Tropical savanna
#> 596                                                                            Tropical seasonal forest
#> 597                                                                                  Tropical thornwood
#> 598                                                                                   Tundra and alpine
#> 599                                                                                            Woodland
#> 600                                                                                            no_biome
#> 601                                                                                              Boreal
#> 602                                                                                   Desert semidesert
#> 603                                                                                   ET Boreal - polar
#> 604                             ET Desert semidesert - tropical subtropical seasonal rainforest savanna
#> 605                                                       ET Desert semidesert - winter cold semidesert
#> 606                                                                ET Laurel forest - desert semidesert
#> 607                                                                    ET Laurel forest - mediterranean
#> 608                                                                          ET Laurel forest - nemoral
#> 609                                                              ET Laurel forest - tropical rainforest
#> 610                                 ET Laurel forest - tropical subtropical seasonal rainforest savanna
#> 611                                                               ET Laurel forest - winter cold steppe
#> 612                                                                ET Mediterranean - desert semidesert
#> 613                                                                          ET Mediterranean - nemoral
#> 614                                                               ET Mediterranean - winter cold steppe
#> 615                                                                                 ET Nemoral - boreal
#> 616                                                                                  ET Nemoral - polar
#> 617                                                                     ET Nemoral - winter cold steppe
#> 618                                                          ET Tropical rainforest - desert semidesert
#> 619                           ET Tropical rainforest - tropical subtropical seasonal rainforest savanna
#> 620                                                                      ET Winter cold steppe - boreal
#> 621                                                                       ET Winter cold steppe - polar
#> 622                               ET Winter cold steppe - tropical subtropical seasonal rainforest sava
#> 623                                                                                        Inland water
#> 624                                                                                       Laurel forest
#> 625                                                                                       Mediterranean
#> 626                                                                                             Nemoral
#> 627                                                                                     Oceanic islands
#> 628                                                                                               Polar
#> 629                                                                                 Tropical rainforest
#> 630                                                    Tropical subtropical seasonal rainforest savanna
#> 631                                                                                  Winter cold desert
#> 632                                                                              Winter cold semidesert
#> 633                                                                                  Winter cold steppe
#> 634                                                                                            no_biome
#>         n
#> 1    2217
#> 2     367
#> 3     384
#> 4     221
#> 5     447
#> 6     583
#> 7     483
#> 8     148
#> 9    5171
#> 10   1687
#> 11    323
#> 12    407
#> 13    729
#> 14   6139
#> 15   1860
#> 16    118
#> 17    824
#> 18      2
#> 19   2342
#> 20   4652
#> 21    332
#> 22   2301
#> 23      1
#> 24   3049
#> 25   3707
#> 26    190
#> 27     86
#> 28   8829
#> 29   3186
#> 30     67
#> 31    135
#> 32     71
#> 33     17
#> 34   2899
#> 35   1092
#> 36      4
#> 37   1621
#> 38   1517
#> 39    976
#> 40    854
#> 41    922
#> 42   4523
#> 43    818
#> 44    479
#> 45    175
#> 46   1851
#> 47   6757
#> 48    167
#> 49   1767
#> 50   2153
#> 51    257
#> 52    130
#> 53    779
#> 54   2599
#> 55   2195
#> 56      2
#> 57     85
#> 58     54
#> 59     60
#> 60      9
#> 61      7
#> 62      2
#> 63    402
#> 64   1081
#> 65     12
#> 66    136
#> 67   1545
#> 68   5090
#> 69    829
#> 70    391
#> 71    152
#> 72     16
#> 73     72
#> 74   1347
#> 75   3873
#> 76    327
#> 77   2380
#> 78    749
#> 79    366
#> 80    387
#> 81    203
#> 82   7760
#> 83   1903
#> 84   1566
#> 85   1124
#> 86    979
#> 87    143
#> 88    105
#> 89   3621
#> 90    205
#> 91      2
#> 92  10404
#> 93   1818
#> 94   1229
#> 95    170
#> 96    768
#> 97   5155
#> 98   1618
#> 99    602
#> 100  1161
#> 101     3
#> 102   727
#> 103     2
#> 104     2
#> 105   725
#> 106   681
#> 107   227
#> 108   274
#> 109  2524
#> 110  1145
#> 111 10348
#> 112  1760
#> 113  2518
#> 114  1683
#> 115  6485
#> 116  2124
#> 117  1736
#> 118   941
#> 119  1198
#> 120   132
#> 121  9429
#> 122   365
#> 123   423
#> 124  2867
#> 125  1800
#> 126  1385
#> 127   403
#> 128  6301
#> 129     3
#> 130  1046
#> 131   800
#> 132  2002
#> 133   410
#> 134   328
#> 135  9243
#> 136   357
#> 137   463
#> 138  3752
#> 139  1501
#> 140  1803
#> 141  1095
#> 142  6301
#> 143     2
#> 144     2
#> 145   395
#> 146   714
#> 147   511
#> 148   341
#> 149   369
#> 150   797
#> 151   460
#> 152   272
#> 153  2167
#> 154  6499
#> 155    20
#> 156    28
#> 157  1094
#> 158  3088
#> 159    71
#> 160    77
#> 161    17
#> 162    44
#> 163  1097
#> 164  3877
#> 165  1850
#> 166  5250
#> 167    62
#> 168   609
#> 169   252
#> 170   879
#> 171   127
#> 172   131
#> 173    45
#> 174  1292
#> 175   715
#> 176   681
#> 177   621
#> 178     1
#> 179    34
#> 180   473
#> 181  3029
#> 182     1
#> 183    51
#> 184    60
#> 185   221
#> 186     3
#> 187   230
#> 188    36
#> 189   343
#> 190    78
#> 191  4724
#> 192   524
#> 193  7623
#> 194    51
#> 195  5174
#> 196   179
#> 197     6
#> 198   150
#> 199   761
#> 200  5150
#> 201     4
#> 202    95
#> 203  2098
#> 204  2533
#> 205  2789
#> 206  8765
#> 207   662
#> 208  4079
#> 209   509
#> 210  1032
#> 211  1348
#> 212    40
#> 213     1
#> 214  3438
#> 215  1114
#> 216   767
#> 217  6281
#> 218   238
#> 219   673
#> 220     8
#> 221    32
#> 222  1767
#> 223   457
#> 224   121
#> 225  1812
#> 226  4951
#> 227  4066
#> 228  2314
#> 229  1064
#> 230  1148
#> 231   774
#> 232     5
#> 233     6
#> 234     5
#> 235   494
#> 236  1712
#> 237  1187
#> 238  2392
#> 239  5399
#> 240  1299
#> 241    73
#> 242  3135
#> 243  6210
#> 244   342
#> 245    72
#> 246   707
#> 247   980
#> 248   723
#> 249   973
#> 250   308
#> 251  1160
#> 252    68
#> 253    26
#> 254  1979
#> 255  3513
#> 256  6191
#> 257  2647
#> 258  1529
#> 259   906
#> 260    62
#> 261  1326
#> 262   425
#> 263  2226
#> 264    87
#> 265  1373
#> 266    39
#> 267   300
#> 268  2685
#> 269  2208
#> 270    96
#> 271  1418
#> 272    65
#> 273   384
#> 274  2432
#> 275   147
#> 276   937
#> 277   457
#> 278   812
#> 279  1586
#> 280   833
#> 281    47
#> 282 11558
#> 283  3384
#> 284  6462
#> 285   545
#> 286   367
#> 287   850
#> 288   296
#> 289  4461
#> 290  1738
#> 291  2988
#> 292  2202
#> 293   290
#> 294  2234
#> 295   383
#> 296   430
#> 297  2944
#> 298  1994
#> 299  2397
#> 300    83
#> 301  3786
#> 302   108
#> 303   824
#> 304   184
#> 305   180
#> 306  3348
#> 307   106
#> 308  2031
#> 309   994
#> 310  2608
#> 311    34
#> 312   174
#> 313  1593
#> 314  1232
#> 315  1756
#> 316   905
#> 317  1153
#> 318  2797
#> 319   622
#> 320   604
#> 321    86
#> 322  5620
#> 323     9
#> 324   709
#> 325  1087
#> 326  1456
#> 327   254
#> 328   184
#> 329  2917
#> 330   594
#> 331   588
#> 332  2760
#> 333  1212
#> 334  7222
#> 335  1907
#> 336   721
#> 337    83
#> 338  4058
#> 339     6
#> 340  2595
#> 341  2845
#> 342  1158
#> 343  1156
#> 344  1002
#> 345   139
#> 346     3
#> 347   100
#> 348  3874
#> 349   230
#> 350     1
#> 351 10595
#> 352  1851
#> 353   805
#> 354   124
#> 355   773
#> 356  5076
#> 357  1683
#> 358   593
#> 359  1099
#> 360   425
#> 361   249
#> 362   744
#> 363   702
#> 364   723
#> 365   483
#> 366   151
#> 367  5891
#> 368  7407
#> 369   120
#> 370  1346
#> 371  3127
#> 372   333
#> 373   239
#> 374   582
#> 375  2536
#> 376  1418
#> 377    16
#> 378    65
#> 379    13
#> 380    40
#> 381     9
#> 382     2
#> 383     1
#> 384    86
#> 385  1181
#> 386   366
#> 387   849
#> 388   338
#> 389  4749
#> 390  6143
#> 391   820
#> 392   608
#> 393    27
#> 394   167
#> 395  1383
#> 396    86
#> 397     8
#> 398   887
#> 399  2495
#> 400  1005
#> 401  1879
#> 402     2
#> 403  1220
#> 404     1
#> 405  4101
#> 406     8
#> 407    56
#> 408  1623
#> 409  1498
#> 410   242
#> 411    83
#> 412   368
#> 413  1252
#> 414     1
#> 415   303
#> 416   698
#> 417   260
#> 418  1717
#> 419   289
#> 420     1
#> 421   438
#> 422     1
#> 423    14
#> 424   276
#> 425  1288
#> 426  6308
#> 427   516
#> 428  2389
#> 429  1087
#> 430   400
#> 431  1483
#> 432     5
#> 433   316
#> 434   475
#> 435   438
#> 436  2717
#> 437  5739
#> 438  1120
#> 439  1007
#> 440   100
#> 441    17
#> 442   100
#> 443  3648
#> 444   212
#> 445     2
#> 446 10540
#> 447  1931
#> 448  1166
#> 449   178
#> 450   765
#> 451  4685
#> 452  1697
#> 453   606
#> 454  1330
#> 455   213
#> 456   853
#> 457  3290
#> 458  3778
#> 459   611
#> 460     3
#> 461  2579
#> 462  1740
#> 463  5193
#> 464    41
#> 465  2149
#> 466  1057
#> 467   994
#> 468  2060
#> 469   139
#> 470  3384
#> 471  1020
#> 472  1775
#> 473  2415
#> 474    94
#> 475  1296
#> 476  3187
#> 477   812
#> 478  7265
#> 479  5731
#> 480  3529
#> 481   339
#> 482  2350
#> 483   192
#> 484   119
#> 485     4
#> 486     9
#> 487   368
#> 488   959
#> 489  1208
#> 490    15
#> 491   446
#> 492  7450
#> 493   373
#> 494   932
#> 495  1339
#> 496     1
#> 497    95
#> 498   228
#> 499     1
#> 500     8
#> 501   542
#> 502   157
#> 503    37
#> 504   341
#> 505  1919
#> 506  1219
#> 507    11
#> 508   449
#> 509   109
#> 510     6
#> 511   906
#> 512   363
#> 513     5
#> 514    94
#> 515    26
#> 516    10
#> 517   200
#> 518  3055
#> 519   533
#> 520   898
#> 521    23
#> 522  4765
#> 523   605
#> 524  2118
#> 525   842
#> 526   752
#> 527    39
#> 528 11073
#> 529   530
#> 530  2644
#> 531   237
#> 532  4541
#> 533    16
#> 534   203
#> 535   321
#> 536     8
#> 537   253
#> 538  4161
#> 539   761
#> 540   574
#> 541   258
#> 542  1925
#> 543  1885
#> 544   118
#> 545   509
#> 546   230
#> 547    28
#> 548  4059
#> 549   731
#> 550 14346
#> 551  1441
#> 552     8
#> 553  1501
#> 554   730
#> 555   761
#> 556   133
#> 557    92
#> 558   226
#> 559   398
#> 560  1291
#> 561    41
#> 562  1255
#> 563   591
#> 564   454
#> 565   150
#> 566   230
#> 567    51
#> 568  4893
#> 569   128
#> 570     1
#> 571   249
#> 572    29
#> 573  9241
#> 574   740
#> 575   168
#> 576   570
#> 577    24
#> 578  4368
#> 579   406
#> 580   157
#> 581   768
#> 582   871
#> 583   377
#> 584   419
#> 585    22
#> 586   761
#> 587  1197
#> 588  4884
#> 589    23
#> 590  4282
#> 591   237
#> 592 13310
#> 593   527
#> 594   731
#> 595   846
#> 596   443
#> 597   417
#> 598  1210
#> 599   236
#> 600   761
#> 601   594
#> 602   249
#> 603   652
#> 604  4424
#> 605     7
#> 606   116
#> 607   891
#> 608  1623
#> 609     6
#> 610   289
#> 611   368
#> 612   741
#> 613    66
#> 614    34
#> 615   979
#> 616     8
#> 617    54
#> 618    32
#> 619   699
#> 620    12
#> 621   112
#> 622    17
#> 623    48
#> 624  2870
#> 625  3347
#> 626  6172
#> 627   237
#> 628    14
#> 629  1490
#> 630  1574
#> 631    62
#> 632   160
#> 633   396
#> 634   761

# Tabulate by raster value
classified_ids <- biomes_classify(
  x     = biomes_example,
  value = "ID"
)
#> no biome file or layer provided using default biomes
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 31 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
#>   - Biome_Inventory_layer_02 (Buchhorn et al., 2019)
#>   - Biome_Inventory_layer_03 (Beck et al., 2018)
#>   - Biome_Inventory_layer_04 (Hengl et al., 2018)
#>   - Biome_Inventory_layer_05 (Dinerstein et al., 2017)
#>   - Biome_Inventory_layer_06 (Zhang et al., 2017)
#>   - Biome_Inventory_layer_07 (Netzel & Stepinski, 2016a)
#>   - Biome_Inventory_layer_08 (Netzel & Stepinski, 2016b)
#>   - Biome_Inventory_layer_09 (Higgins et al., 2016)
#>   - Biome_Inventory_layer_10 (Pfadenhauer & Klötzli, 2014)
#>   - Biome_Inventory_layer_11 (Zhang & Yan, 2014)
#>   - Biome_Inventory_layer_12 (Metzger et al., 2013)
#>   - Biome_Inventory_layer_13 (Food and Agriculture Organization of the United Nations, 2012)
#>   - Biome_Inventory_layer_14 (Tateishi et al., 2011; Tateishi et al., 2014; Kobayashi et al., 2017)
#>   - Biome_Inventory_layer_15 (Defries et al., 2010)
#>   - Biome_Inventory_layer_16 (Ellis et al., 2010)
#>   - Biome_Inventory_layer_17 (European Space Agency, 2010)
#>   - Biome_Inventory_layer_18 (Friedl et al., 2010)
#>   - Biome_Inventory_layer_19 (The Nature Conservancy, 2009)
#>   - Biome_Inventory_layer_20 (Peel et al., 2007)
#>   - Biome_Inventory_layer_21 (Bartholomé & Belward, 2005)
#>   - Biome_Inventory_layer_22 (Kaplan et al., 2003)
#>   - Biome_Inventory_layer_23 (Olson et al., 2001)
#>   - Biome_Inventory_layer_24 (Loveland et al., 2000)
#>   - Biome_Inventory_layer_25 (Ramankutty & Foley, 1999)
#>   - Biome_Inventory_layer_26 (Leemans, 1990)
#>   - Biome_Inventory_layer_27 (Schultz, 1988, 1995, 2002, 2008, 2016)
#>   - Biome_Inventory_layer_28 (Müller-Hohenstein, 1981)
#>   - Biome_Inventory_layer_29 (Schmithüsen, 1976)
#>   - Biome_Inventory_layer_30 (Whittaker, 1975)
#>   - Biome_Inventory_layer_31 (Walter, 1964, 1968; Walter & Breckle, 1970; Breckle & Rafiqpoor, 2019)
biomes_tab(classified_ids, value = "ID")
#>                        layer biome     n
#> 1   Biome_Inventory_layer_01     1  1860
#> 2   Biome_Inventory_layer_01     2   824
#> 3   Biome_Inventory_layer_01     3   447
#> 4   Biome_Inventory_layer_01     4   118
#> 5   Biome_Inventory_layer_01     5  2342
#> 6   Biome_Inventory_layer_01     6   221
#> 7   Biome_Inventory_layer_01     7  5171
#> 8   Biome_Inventory_layer_01     8   583
#> 9   Biome_Inventory_layer_01     9   729
#> 10  Biome_Inventory_layer_01    10   323
#> 11  Biome_Inventory_layer_01    11   148
#> 12  Biome_Inventory_layer_01    12   407
#> 13  Biome_Inventory_layer_01    13  6139
#> 14  Biome_Inventory_layer_01    14  1687
#> 15  Biome_Inventory_layer_01    15   367
#> 16  Biome_Inventory_layer_01    16     2
#> 17  Biome_Inventory_layer_01    17   384
#> 18  Biome_Inventory_layer_01    18  2217
#> 19  Biome_Inventory_layer_01    20   483
#> 20  Biome_Inventory_layer_02     1  3049
#> 21  Biome_Inventory_layer_02     2    71
#> 22  Biome_Inventory_layer_02     3  2899
#> 23  Biome_Inventory_layer_02     4  1092
#> 24  Biome_Inventory_layer_02     5   332
#> 25  Biome_Inventory_layer_02     6  8829
#> 26  Biome_Inventory_layer_02     7  2301
#> 27  Biome_Inventory_layer_02     8    86
#> 28  Biome_Inventory_layer_02    10  3186
#> 29  Biome_Inventory_layer_02    11    17
#> 30  Biome_Inventory_layer_02    12   190
#> 31  Biome_Inventory_layer_02    13  3707
#> 32  Biome_Inventory_layer_02    14    67
#> 33  Biome_Inventory_layer_02    15     1
#> 34  Biome_Inventory_layer_02    17     4
#> 35  Biome_Inventory_layer_02    95   135
#> 36  Biome_Inventory_layer_02    98  1621
#> 37  Biome_Inventory_layer_03     1   976
#> 38  Biome_Inventory_layer_03     2   854
#> 39  Biome_Inventory_layer_03     3   922
#> 40  Biome_Inventory_layer_03     5  4523
#> 41  Biome_Inventory_layer_03     6   130
#> 42  Biome_Inventory_layer_03     7   257
#> 43  Biome_Inventory_layer_03     8   479
#> 44  Biome_Inventory_layer_03     9  1851
#> 45  Biome_Inventory_layer_03    10  2153
#> 46  Biome_Inventory_layer_03    11     2
#> 47  Biome_Inventory_layer_03    13  6757
#> 48  Biome_Inventory_layer_03    14  1767
#> 49  Biome_Inventory_layer_03    15   175
#> 50  Biome_Inventory_layer_03    16    85
#> 51  Biome_Inventory_layer_03    17   818
#> 52  Biome_Inventory_layer_03    18    60
#> 53  Biome_Inventory_layer_03    19   779
#> 54  Biome_Inventory_layer_03    20     9
#> 55  Biome_Inventory_layer_03    21   167
#> 56  Biome_Inventory_layer_03    22  2599
#> 57  Biome_Inventory_layer_03    23     7
#> 58  Biome_Inventory_layer_03    24   402
#> 59  Biome_Inventory_layer_03    25  2195
#> 60  Biome_Inventory_layer_03    26    54
#> 61  Biome_Inventory_layer_03    30     2
#> 62  Biome_Inventory_layer_04     1   366
#> 63  Biome_Inventory_layer_04     2   203
#> 64  Biome_Inventory_layer_04     3   387
#> 65  Biome_Inventory_layer_04     4   749
#> 66  Biome_Inventory_layer_04     5  1903
#> 67  Biome_Inventory_layer_04     6  7760
#> 68  Biome_Inventory_layer_04     7  1347
#> 69  Biome_Inventory_layer_04     8  2380
#> 70  Biome_Inventory_layer_04     9   391
#> 71  Biome_Inventory_layer_04    10   327
#> 72  Biome_Inventory_layer_04    11  3873
#> 73  Biome_Inventory_layer_04    12   829
#> 74  Biome_Inventory_layer_04    13  5090
#> 75  Biome_Inventory_layer_04    14    16
#> 76  Biome_Inventory_layer_04    15   136
#> 77  Biome_Inventory_layer_04    16  1545
#> 78  Biome_Inventory_layer_04    17    12
#> 79  Biome_Inventory_layer_04    18    72
#> 80  Biome_Inventory_layer_04    19   152
#> 81  Biome_Inventory_layer_05     1  1618
#> 82  Biome_Inventory_layer_05     2   105
#> 83  Biome_Inventory_layer_05     3  5155
#> 84  Biome_Inventory_layer_05     4   768
#> 85  Biome_Inventory_layer_05     5   143
#> 86  Biome_Inventory_layer_05     6   170
#> 87  Biome_Inventory_layer_05     7   979
#> 88  Biome_Inventory_layer_05     8   205
#> 89  Biome_Inventory_layer_05     9  3621
#> 90  Biome_Inventory_layer_05    10  1229
#> 91  Biome_Inventory_layer_05    11 10404
#> 92  Biome_Inventory_layer_05    12  1818
#> 93  Biome_Inventory_layer_05    13  1124
#> 94  Biome_Inventory_layer_05    14   602
#> 95  Biome_Inventory_layer_05    15     2
#> 96  Biome_Inventory_layer_06     1  1760
#> 97  Biome_Inventory_layer_06     2  1683
#> 98  Biome_Inventory_layer_06     3  2518
#> 99  Biome_Inventory_layer_06     4  1145
#> 100 Biome_Inventory_layer_06     5   227
#> 101 Biome_Inventory_layer_06     6 10348
#> 102 Biome_Inventory_layer_06     7   274
#> 103 Biome_Inventory_layer_06     8   681
#> 104 Biome_Inventory_layer_06     9  2524
#> 105 Biome_Inventory_layer_06    10   725
#> 106 Biome_Inventory_layer_06    11   727
#> 107 Biome_Inventory_layer_06    12     3
#> 108 Biome_Inventory_layer_06    13     2
#> 109 Biome_Inventory_layer_06    14     2
#> 110 Biome_Inventory_layer_07     1   423
#> 111 Biome_Inventory_layer_07     2   403
#> 112 Biome_Inventory_layer_07     3  1800
#> 113 Biome_Inventory_layer_07     4  1736
#> 114 Biome_Inventory_layer_07     5  2124
#> 115 Biome_Inventory_layer_07     6   941
#> 116 Biome_Inventory_layer_07     7  1198
#> 117 Biome_Inventory_layer_07     8  2867
#> 118 Biome_Inventory_layer_07     9  1385
#> 119 Biome_Inventory_layer_07    10   365
#> 120 Biome_Inventory_layer_07    11  9429
#> 121 Biome_Inventory_layer_07    12   132
#> 122 Biome_Inventory_layer_08     1   463
#> 123 Biome_Inventory_layer_08     2  1501
#> 124 Biome_Inventory_layer_08     3  1095
#> 125 Biome_Inventory_layer_08     4  1046
#> 126 Biome_Inventory_layer_08     5   410
#> 127 Biome_Inventory_layer_08     6   800
#> 128 Biome_Inventory_layer_08     7  2002
#> 129 Biome_Inventory_layer_08     8  3752
#> 130 Biome_Inventory_layer_08     9  1803
#> 131 Biome_Inventory_layer_08    10   357
#> 132 Biome_Inventory_layer_08    11  9243
#> 133 Biome_Inventory_layer_08    12   328
#> 134 Biome_Inventory_layer_08    13     3
#> 135 Biome_Inventory_layer_09     1  3088
#> 136 Biome_Inventory_layer_09     2  1094
#> 137 Biome_Inventory_layer_09     3   395
#> 138 Biome_Inventory_layer_09     4   714
#> 139 Biome_Inventory_layer_09     5  2167
#> 140 Biome_Inventory_layer_09     6  1850
#> 141 Biome_Inventory_layer_09     7   369
#> 142 Biome_Inventory_layer_09     8   797
#> 143 Biome_Inventory_layer_09     9    20
#> 144 Biome_Inventory_layer_09    10    17
#> 145 Biome_Inventory_layer_09    11     2
#> 146 Biome_Inventory_layer_09    12    44
#> 147 Biome_Inventory_layer_09    13  6499
#> 148 Biome_Inventory_layer_09    14  5250
#> 149 Biome_Inventory_layer_09    15     2
#> 150 Biome_Inventory_layer_09    16    28
#> 151 Biome_Inventory_layer_09    17   460
#> 152 Biome_Inventory_layer_09    18  1097
#> 153 Biome_Inventory_layer_09    19   511
#> 154 Biome_Inventory_layer_09    20   272
#> 155 Biome_Inventory_layer_09    21  3877
#> 156 Biome_Inventory_layer_09    22   341
#> 157 Biome_Inventory_layer_09    23    71
#> 158 Biome_Inventory_layer_09    24    77
#> 159 Biome_Inventory_layer_10     1   609
#> 160 Biome_Inventory_layer_10     2    36
#> 161 Biome_Inventory_layer_10     3   131
#> 162 Biome_Inventory_layer_10     4   230
#> 163 Biome_Inventory_layer_10     5   343
#> 164 Biome_Inventory_layer_10     6  5174
#> 165 Biome_Inventory_layer_10     7   127
#> 166 Biome_Inventory_layer_10     8   179
#> 167 Biome_Inventory_layer_10     9     6
#> 168 Biome_Inventory_layer_10    10    51
#> 169 Biome_Inventory_layer_10    11   681
#> 170 Biome_Inventory_layer_10    12   150
#> 171 Biome_Inventory_layer_10    13     1
#> 172 Biome_Inventory_layer_10    14    78
#> 173 Biome_Inventory_layer_10    15  4724
#> 174 Biome_Inventory_layer_10    16    51
#> 175 Biome_Inventory_layer_10    17     1
#> 176 Biome_Inventory_layer_10    18    60
#> 177 Biome_Inventory_layer_10    19   715
#> 178 Biome_Inventory_layer_10    20  7623
#> 179 Biome_Inventory_layer_10    21   473
#> 180 Biome_Inventory_layer_10    22   252
#> 181 Biome_Inventory_layer_10    23    45
#> 182 Biome_Inventory_layer_10    24  1292
#> 183 Biome_Inventory_layer_10    25   621
#> 184 Biome_Inventory_layer_10    26   879
#> 185 Biome_Inventory_layer_10    28   524
#> 186 Biome_Inventory_layer_10    29     3
#> 187 Biome_Inventory_layer_10    95    34
#> 188 Biome_Inventory_layer_10    96   221
#> 189 Biome_Inventory_layer_10    97  3029
#> 190 Biome_Inventory_layer_11     1   509
#> 191 Biome_Inventory_layer_11     2  1348
#> 192 Biome_Inventory_layer_11     3  1032
#> 193 Biome_Inventory_layer_11     4   662
#> 194 Biome_Inventory_layer_11     5  4079
#> 195 Biome_Inventory_layer_11     6  2789
#> 196 Biome_Inventory_layer_11     7  5150
#> 197 Biome_Inventory_layer_11     8  2533
#> 198 Biome_Inventory_layer_11     9  8765
#> 199 Biome_Inventory_layer_11    10  2098
#> 200 Biome_Inventory_layer_11    11    95
#> 201 Biome_Inventory_layer_11    13     4
#> 202 Biome_Inventory_layer_12     1  1767
#> 203 Biome_Inventory_layer_12     2  4951
#> 204 Biome_Inventory_layer_12     3   457
#> 205 Biome_Inventory_layer_12     4    32
#> 206 Biome_Inventory_layer_12     5  1812
#> 207 Biome_Inventory_layer_12     6   121
#> 208 Biome_Inventory_layer_12     7  2314
#> 209 Biome_Inventory_layer_12     8  4066
#> 210 Biome_Inventory_layer_12     9  6281
#> 211 Biome_Inventory_layer_12    10   238
#> 212 Biome_Inventory_layer_12    11   767
#> 213 Biome_Inventory_layer_12    12  3438
#> 214 Biome_Inventory_layer_12    13  1114
#> 215 Biome_Inventory_layer_12    14   673
#> 216 Biome_Inventory_layer_12    15     8
#> 217 Biome_Inventory_layer_12    16     1
#> 218 Biome_Inventory_layer_13     1   973
#> 219 Biome_Inventory_layer_13     2   980
#> 220 Biome_Inventory_layer_13     3   723
#> 221 Biome_Inventory_layer_13     4   308
#> 222 Biome_Inventory_layer_13     5   707
#> 223 Biome_Inventory_layer_13     6    72
#> 224 Biome_Inventory_layer_13     7   494
#> 225 Biome_Inventory_layer_13     8  1187
#> 226 Biome_Inventory_layer_13     9  2392
#> 227 Biome_Inventory_layer_13    10  5399
#> 228 Biome_Inventory_layer_13    11  1712
#> 229 Biome_Inventory_layer_13    12  3135
#> 230 Biome_Inventory_layer_13    13    73
#> 231 Biome_Inventory_layer_13    14   342
#> 232 Biome_Inventory_layer_13    15  1299
#> 233 Biome_Inventory_layer_13    16  6210
#> 234 Biome_Inventory_layer_13    17   774
#> 235 Biome_Inventory_layer_13    18  1148
#> 236 Biome_Inventory_layer_13    19     5
#> 237 Biome_Inventory_layer_13    20     5
#> 238 Biome_Inventory_layer_13    95     6
#> 239 Biome_Inventory_layer_14     1    62
#> 240 Biome_Inventory_layer_14     2  3513
#> 241 Biome_Inventory_layer_14     3   906
#> 242 Biome_Inventory_layer_14     4    26
#> 243 Biome_Inventory_layer_14     5    87
#> 244 Biome_Inventory_layer_14     6  2647
#> 245 Biome_Inventory_layer_14     7  1373
#> 246 Biome_Inventory_layer_14     8  1979
#> 247 Biome_Inventory_layer_14     9    68
#> 248 Biome_Inventory_layer_14    10  2685
#> 249 Biome_Inventory_layer_14    11    96
#> 250 Biome_Inventory_layer_14    12   300
#> 251 Biome_Inventory_layer_14    13  6191
#> 252 Biome_Inventory_layer_14    14  1529
#> 253 Biome_Inventory_layer_14    15  2226
#> 254 Biome_Inventory_layer_14    16  1326
#> 255 Biome_Inventory_layer_14    17   425
#> 256 Biome_Inventory_layer_14    18    39
#> 257 Biome_Inventory_layer_14    98  2208
#> 258 Biome_Inventory_layer_15     1   937
#> 259 Biome_Inventory_layer_15     2 11558
#> 260 Biome_Inventory_layer_15     3  3384
#> 261 Biome_Inventory_layer_15     4   147
#> 262 Biome_Inventory_layer_15     5   812
#> 263 Biome_Inventory_layer_15     6  2432
#> 264 Biome_Inventory_layer_15     7   833
#> 265 Biome_Inventory_layer_15     8   384
#> 266 Biome_Inventory_layer_15     9    65
#> 267 Biome_Inventory_layer_15    10  1586
#> 268 Biome_Inventory_layer_15    11   457
#> 269 Biome_Inventory_layer_15    98    47
#> 270 Biome_Inventory_layer_16     1  1994
#> 271 Biome_Inventory_layer_16     2   296
#> 272 Biome_Inventory_layer_16     3  2397
#> 273 Biome_Inventory_layer_16     4    83
#> 274 Biome_Inventory_layer_16     5  1738
#> 275 Biome_Inventory_layer_16     6   383
#> 276 Biome_Inventory_layer_16     7  2988
#> 277 Biome_Inventory_layer_16     8  2202
#> 278 Biome_Inventory_layer_16     9   850
#> 279 Biome_Inventory_layer_16    10   545
#> 280 Biome_Inventory_layer_16    11  2234
#> 281 Biome_Inventory_layer_16    12   367
#> 282 Biome_Inventory_layer_16    13  2944
#> 283 Biome_Inventory_layer_16    14   430
#> 284 Biome_Inventory_layer_16    15  4461
#> 285 Biome_Inventory_layer_16    16   290
#> 286 Biome_Inventory_layer_16    17   108
#> 287 Biome_Inventory_layer_16    18   824
#> 288 Biome_Inventory_layer_16    98  3786
#> 289 Biome_Inventory_layer_17     1    34
#> 290 Biome_Inventory_layer_17     2  2608
#> 291 Biome_Inventory_layer_17     3   622
#> 292 Biome_Inventory_layer_17     4   106
#> 293 Biome_Inventory_layer_17     5   994
#> 294 Biome_Inventory_layer_17     6  2797
#> 295 Biome_Inventory_layer_17     7  1756
#> 296 Biome_Inventory_layer_17     8    86
#> 297 Biome_Inventory_layer_17     9  1593
#> 298 Biome_Inventory_layer_17    10   180
#> 299 Biome_Inventory_layer_17    11  5620
#> 300 Biome_Inventory_layer_17    12   905
#> 301 Biome_Inventory_layer_17    13  3348
#> 302 Biome_Inventory_layer_17    14  2031
#> 303 Biome_Inventory_layer_17    15  1153
#> 304 Biome_Inventory_layer_17    16   174
#> 305 Biome_Inventory_layer_17    17   709
#> 306 Biome_Inventory_layer_17    18  1232
#> 307 Biome_Inventory_layer_17    19   604
#> 308 Biome_Inventory_layer_17    20     9
#> 309 Biome_Inventory_layer_17    98  1087
#> 310 Biome_Inventory_layer_18     1  2760
#> 311 Biome_Inventory_layer_18     2   594
#> 312 Biome_Inventory_layer_18     3   184
#> 313 Biome_Inventory_layer_18     4   254
#> 314 Biome_Inventory_layer_18     5  4058
#> 315 Biome_Inventory_layer_18     6  7222
#> 316 Biome_Inventory_layer_18     7  2917
#> 317 Biome_Inventory_layer_18     8   588
#> 318 Biome_Inventory_layer_18     9  2845
#> 319 Biome_Inventory_layer_18    10   721
#> 320 Biome_Inventory_layer_18    11    83
#> 321 Biome_Inventory_layer_18    12  1907
#> 322 Biome_Inventory_layer_18    13  1212
#> 323 Biome_Inventory_layer_18    15     6
#> 324 Biome_Inventory_layer_18    98  2595
#> 325 Biome_Inventory_layer_19     1  1683
#> 326 Biome_Inventory_layer_19     2   100
#> 327 Biome_Inventory_layer_19     3  5076
#> 328 Biome_Inventory_layer_19     4   773
#> 329 Biome_Inventory_layer_19     5   124
#> 330 Biome_Inventory_layer_19     6   139
#> 331 Biome_Inventory_layer_19     7  1002
#> 332 Biome_Inventory_layer_19     8   230
#> 333 Biome_Inventory_layer_19     9  3874
#> 334 Biome_Inventory_layer_19    10 10595
#> 335 Biome_Inventory_layer_19    11   805
#> 336 Biome_Inventory_layer_19    12  1851
#> 337 Biome_Inventory_layer_19    13  1156
#> 338 Biome_Inventory_layer_19    14   593
#> 339 Biome_Inventory_layer_19    15     1
#> 340 Biome_Inventory_layer_19    95     3
#> 341 Biome_Inventory_layer_20     1   425
#> 342 Biome_Inventory_layer_20     2   249
#> 343 Biome_Inventory_layer_20     3   744
#> 344 Biome_Inventory_layer_20     5   702
#> 345 Biome_Inventory_layer_20     6   239
#> 346 Biome_Inventory_layer_20     7   333
#> 347 Biome_Inventory_layer_20     8   483
#> 348 Biome_Inventory_layer_20     9  5891
#> 349 Biome_Inventory_layer_20    10  3127
#> 350 Biome_Inventory_layer_20    11  1346
#> 351 Biome_Inventory_layer_20    12   366
#> 352 Biome_Inventory_layer_20    13   151
#> 353 Biome_Inventory_layer_20    14  7407
#> 354 Biome_Inventory_layer_20    15   723
#> 355 Biome_Inventory_layer_20    16    16
#> 356 Biome_Inventory_layer_20    17    40
#> 357 Biome_Inventory_layer_20    18     9
#> 358 Biome_Inventory_layer_20    19   582
#> 359 Biome_Inventory_layer_20    20    65
#> 360 Biome_Inventory_layer_20    21    86
#> 361 Biome_Inventory_layer_20    22   120
#> 362 Biome_Inventory_layer_20    23     2
#> 363 Biome_Inventory_layer_20    24  2536
#> 364 Biome_Inventory_layer_20    25    13
#> 365 Biome_Inventory_layer_20    26  1418
#> 366 Biome_Inventory_layer_20    30  1181
#> 367 Biome_Inventory_layer_20    31     1
#> 368 Biome_Inventory_layer_21     1     8
#> 369 Biome_Inventory_layer_21     2  1879
#> 370 Biome_Inventory_layer_21     3    56
#> 371 Biome_Inventory_layer_21     4  1005
#> 372 Biome_Inventory_layer_21     5   608
#> 373 Biome_Inventory_layer_21     6   820
#> 374 Biome_Inventory_layer_21     7   338
#> 375 Biome_Inventory_layer_21     8  6143
#> 376 Biome_Inventory_layer_21     9  1383
#> 377 Biome_Inventory_layer_21    10  4749
#> 378 Biome_Inventory_layer_21    11  2495
#> 379 Biome_Inventory_layer_21    12    86
#> 380 Biome_Inventory_layer_21    13   887
#> 381 Biome_Inventory_layer_21    14    27
#> 382 Biome_Inventory_layer_21    15   167
#> 383 Biome_Inventory_layer_21    16  4101
#> 384 Biome_Inventory_layer_21    17  1220
#> 385 Biome_Inventory_layer_21    18     2
#> 386 Biome_Inventory_layer_21    19     1
#> 387 Biome_Inventory_layer_21    20     8
#> 388 Biome_Inventory_layer_21    98  1623
#> 389 Biome_Inventory_layer_22     1  1483
#> 390 Biome_Inventory_layer_22     2   475
#> 391 Biome_Inventory_layer_22     3   316
#> 392 Biome_Inventory_layer_22     4   400
#> 393 Biome_Inventory_layer_22     5   438
#> 394 Biome_Inventory_layer_22     6     5
#> 395 Biome_Inventory_layer_22     7   242
#> 396 Biome_Inventory_layer_22     8   698
#> 397 Biome_Inventory_layer_22     9  2717
#> 398 Biome_Inventory_layer_22    10  1288
#> 399 Biome_Inventory_layer_22    11  2389
#> 400 Biome_Inventory_layer_22    12   289
#> 401 Biome_Inventory_layer_22    13  1087
#> 402 Biome_Inventory_layer_22    14    14
#> 403 Biome_Inventory_layer_22    15   276
#> 404 Biome_Inventory_layer_22    16  6308
#> 405 Biome_Inventory_layer_22    17   516
#> 406 Biome_Inventory_layer_22    18    83
#> 407 Biome_Inventory_layer_22    19  1252
#> 408 Biome_Inventory_layer_22    20   368
#> 409 Biome_Inventory_layer_22    21  1717
#> 410 Biome_Inventory_layer_22    22   303
#> 411 Biome_Inventory_layer_22    23     1
#> 412 Biome_Inventory_layer_22    24   438
#> 413 Biome_Inventory_layer_22    25   260
#> 414 Biome_Inventory_layer_22    26     1
#> 415 Biome_Inventory_layer_22    27     1
#> 416 Biome_Inventory_layer_23     1  1697
#> 417 Biome_Inventory_layer_23     2   100
#> 418 Biome_Inventory_layer_23     3  4685
#> 419 Biome_Inventory_layer_23     4   765
#> 420 Biome_Inventory_layer_23     5   178
#> 421 Biome_Inventory_layer_23     6   100
#> 422 Biome_Inventory_layer_23     7  1007
#> 423 Biome_Inventory_layer_23     8   212
#> 424 Biome_Inventory_layer_23     9  3648
#> 425 Biome_Inventory_layer_23    10 10540
#> 426 Biome_Inventory_layer_23    11  1931
#> 427 Biome_Inventory_layer_23    12  1166
#> 428 Biome_Inventory_layer_23    13  1120
#> 429 Biome_Inventory_layer_23    14   606
#> 430 Biome_Inventory_layer_23    15     2
#> 431 Biome_Inventory_layer_23    95    17
#> 432 Biome_Inventory_layer_24     1  2579
#> 433 Biome_Inventory_layer_24     2   994
#> 434 Biome_Inventory_layer_24     3  3290
#> 435 Biome_Inventory_layer_24     4   213
#> 436 Biome_Inventory_layer_24     5  3384
#> 437 Biome_Inventory_layer_24     6   611
#> 438 Biome_Inventory_layer_24     7   139
#> 439 Biome_Inventory_layer_24     8   853
#> 440 Biome_Inventory_layer_24     9  5193
#> 441 Biome_Inventory_layer_24    10  3778
#> 442 Biome_Inventory_layer_24    11  1057
#> 443 Biome_Inventory_layer_24    12  2149
#> 444 Biome_Inventory_layer_24    13  1740
#> 445 Biome_Inventory_layer_24    14     3
#> 446 Biome_Inventory_layer_24    15    41
#> 447 Biome_Inventory_layer_24    98  2060
#> 448 Biome_Inventory_layer_25     1  2350
#> 449 Biome_Inventory_layer_25     2   339
#> 450 Biome_Inventory_layer_25     3  7265
#> 451 Biome_Inventory_layer_25     4  2415
#> 452 Biome_Inventory_layer_25     5    94
#> 453 Biome_Inventory_layer_25     6   812
#> 454 Biome_Inventory_layer_25     7  1296
#> 455 Biome_Inventory_layer_25     8  3529
#> 456 Biome_Inventory_layer_25     9  5731
#> 457 Biome_Inventory_layer_25    10  3187
#> 458 Biome_Inventory_layer_25    11   192
#> 459 Biome_Inventory_layer_25    12  1775
#> 460 Biome_Inventory_layer_26     1   363
#> 461 Biome_Inventory_layer_26     2   109
#> 462 Biome_Inventory_layer_26     4    26
#> 463 Biome_Inventory_layer_26     5    11
#> 464 Biome_Inventory_layer_26     6   906
#> 465 Biome_Inventory_layer_26     7  1219
#> 466 Biome_Inventory_layer_26     8    94
#> 467 Biome_Inventory_layer_26     9     5
#> 468 Biome_Inventory_layer_26    10  1919
#> 469 Biome_Inventory_layer_26    12    23
#> 470 Biome_Inventory_layer_26    13     6
#> 471 Biome_Inventory_layer_26    14   449
#> 472 Biome_Inventory_layer_26    15    37
#> 473 Biome_Inventory_layer_26    16   341
#> 474 Biome_Inventory_layer_26    17   533
#> 475 Biome_Inventory_layer_26    18    10
#> 476 Biome_Inventory_layer_26    19  3055
#> 477 Biome_Inventory_layer_26    20   373
#> 478 Biome_Inventory_layer_26    21   898
#> 479 Biome_Inventory_layer_26    22   200
#> 480 Biome_Inventory_layer_26    23  1339
#> 481 Biome_Inventory_layer_26    24    15
#> 482 Biome_Inventory_layer_26    25   446
#> 483 Biome_Inventory_layer_26    26   932
#> 484 Biome_Inventory_layer_26    27   959
#> 485 Biome_Inventory_layer_26    28  7450
#> 486 Biome_Inventory_layer_26    29     4
#> 487 Biome_Inventory_layer_26    30     9
#> 488 Biome_Inventory_layer_26    31   542
#> 489 Biome_Inventory_layer_26    32  1208
#> 490 Biome_Inventory_layer_26    33   368
#> 491 Biome_Inventory_layer_26    34   228
#> 492 Biome_Inventory_layer_26    35     1
#> 493 Biome_Inventory_layer_26    36   157
#> 494 Biome_Inventory_layer_26    37     8
#> 495 Biome_Inventory_layer_26    38     1
#> 496 Biome_Inventory_layer_26    95    95
#> 497 Biome_Inventory_layer_27     1   842
#> 498 Biome_Inventory_layer_27     2   530
#> 499 Biome_Inventory_layer_27     3   752
#> 500 Biome_Inventory_layer_27     4  4541
#> 501 Biome_Inventory_layer_27     5   321
#> 502 Biome_Inventory_layer_27     6  2118
#> 503 Biome_Inventory_layer_27     7   253
#> 504 Biome_Inventory_layer_27     8  4161
#> 505 Biome_Inventory_layer_27     9    16
#> 506 Biome_Inventory_layer_27    10   203
#> 507 Biome_Inventory_layer_27    11 11073
#> 508 Biome_Inventory_layer_27    12   605
#> 509 Biome_Inventory_layer_27    13     8
#> 510 Biome_Inventory_layer_27    95    39
#> 511 Biome_Inventory_layer_27    96   237
#> 512 Biome_Inventory_layer_27    97  2644
#> 513 Biome_Inventory_layer_28     1  1501
#> 514 Biome_Inventory_layer_28     2     8
#> 515 Biome_Inventory_layer_28     3   509
#> 516 Biome_Inventory_layer_28     4   258
#> 517 Biome_Inventory_layer_28     5  1441
#> 518 Biome_Inventory_layer_28     6   574
#> 519 Biome_Inventory_layer_28     7  1885
#> 520 Biome_Inventory_layer_28     8  1925
#> 521 Biome_Inventory_layer_28     9 14346
#> 522 Biome_Inventory_layer_28    10   731
#> 523 Biome_Inventory_layer_28    11  4059
#> 524 Biome_Inventory_layer_28    12   730
#> 525 Biome_Inventory_layer_28    13    28
#> 526 Biome_Inventory_layer_28    95   118
#> 527 Biome_Inventory_layer_28    96   230
#> 528 Biome_Inventory_layer_29     1   377
#> 529 Biome_Inventory_layer_29     2    51
#> 530 Biome_Inventory_layer_29     3   591
#> 531 Biome_Inventory_layer_29     4   871
#> 532 Biome_Inventory_layer_29     5   419
#> 533 Biome_Inventory_layer_29     6   226
#> 534 Biome_Inventory_layer_29     7    24
#> 535 Biome_Inventory_layer_29     8  4368
#> 536 Biome_Inventory_layer_29     9   768
#> 537 Biome_Inventory_layer_29    10    92
#> 538 Biome_Inventory_layer_29    11  1255
#> 539 Biome_Inventory_layer_29    12   128
#> 540 Biome_Inventory_layer_29    13   406
#> 541 Biome_Inventory_layer_29    14  4893
#> 542 Biome_Inventory_layer_29    15   133
#> 543 Biome_Inventory_layer_29    16   398
#> 544 Biome_Inventory_layer_29    17   157
#> 545 Biome_Inventory_layer_29    18  9241
#> 546 Biome_Inventory_layer_29    19   150
#> 547 Biome_Inventory_layer_29    20     1
#> 548 Biome_Inventory_layer_29    21   168
#> 549 Biome_Inventory_layer_29    22   740
#> 550 Biome_Inventory_layer_29    23   454
#> 551 Biome_Inventory_layer_29    24   570
#> 552 Biome_Inventory_layer_29    25  1291
#> 553 Biome_Inventory_layer_29    26    29
#> 554 Biome_Inventory_layer_29    27   249
#> 555 Biome_Inventory_layer_29    28    22
#> 556 Biome_Inventory_layer_29    95    41
#> 557 Biome_Inventory_layer_29    96   230
#> 558 Biome_Inventory_layer_30     1   731
#> 559 Biome_Inventory_layer_30     2   846
#> 560 Biome_Inventory_layer_30     3   417
#> 561 Biome_Inventory_layer_30     4   443
#> 562 Biome_Inventory_layer_30     5   236
#> 563 Biome_Inventory_layer_30     6  4884
#> 564 Biome_Inventory_layer_30     7  4282
#> 565 Biome_Inventory_layer_30     8 13310
#> 566 Biome_Inventory_layer_30     9   527
#> 567 Biome_Inventory_layer_30    10  1197
#> 568 Biome_Inventory_layer_30    11  1210
#> 569 Biome_Inventory_layer_30    95    23
#> 570 Biome_Inventory_layer_30    96   237
#> 571 Biome_Inventory_layer_31     1    32
#> 572 Biome_Inventory_layer_31     2  1490
#> 573 Biome_Inventory_layer_31     3   699
#> 574 Biome_Inventory_layer_31     4  1574
#> 575 Biome_Inventory_layer_31     5  4424
#> 576 Biome_Inventory_layer_31     6   249
#> 577 Biome_Inventory_layer_31     7   289
#> 578 Biome_Inventory_layer_31     8     6
#> 579 Biome_Inventory_layer_31     9   116
#> 580 Biome_Inventory_layer_31    10   741
#> 581 Biome_Inventory_layer_31    11    17
#> 582 Biome_Inventory_layer_31    12  2870
#> 583 Biome_Inventory_layer_31    13   368
#> 584 Biome_Inventory_layer_31    14     7
#> 585 Biome_Inventory_layer_31    15    34
#> 586 Biome_Inventory_layer_31    16  3347
#> 587 Biome_Inventory_layer_31    17    62
#> 588 Biome_Inventory_layer_31    18   891
#> 589 Biome_Inventory_layer_31    19   396
#> 590 Biome_Inventory_layer_31    20   160
#> 591 Biome_Inventory_layer_31    21    66
#> 592 Biome_Inventory_layer_31    22  6172
#> 593 Biome_Inventory_layer_31    23  1623
#> 594 Biome_Inventory_layer_31    24   112
#> 595 Biome_Inventory_layer_31    25    54
#> 596 Biome_Inventory_layer_31    26   979
#> 597 Biome_Inventory_layer_31    27     8
#> 598 Biome_Inventory_layer_31    28   594
#> 599 Biome_Inventory_layer_31    29    12
#> 600 Biome_Inventory_layer_31    30   652
#> 601 Biome_Inventory_layer_31    31    14
#> 602 Biome_Inventory_layer_31    95    48
#> 603 Biome_Inventory_layer_31    96   237
# }
```

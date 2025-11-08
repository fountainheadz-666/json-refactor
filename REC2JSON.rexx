/* REXX *************************************************************
    Name:     RECB2JSON
    Purpose:  Convert variable-length input records to JSON according
              to COBOL copybooks routed by a mapping file.
    Inputs (DDs):
     INDD     - PS, RECFM=V, key at col 41 (len 9)          -optional
     MAPFILE  - PS, LRECL=80; key col1-9, member col40-47   -required
     LAYLIB   - PDS/PDSE with COBOL layouts (copybooks)     -required
     OUTDD    - PS for JSON output                          -optional
    TSO Parms (optional):
     INDD(name) MAPFILE(name) LAYLIB(name) OUTDD(name)
     SHOWFILLER(0|1)    -> default 1 (print FILLER lines)
     SUMMARY(member)    -> print one member-s summary, if loaded
*********************************************************************/
Address TSO
Numeric Digits 30
/* ------------ Parse optional overrides via TSO parms ------------- */
parse upper arg argstr
inDD = 'INDD'
mapDD = 'MAPFILE'
layPDSDD = 'LAYLIB'
outDD = 'OUTDD'
showFillerDefault = 1
summaryMember     = ''
/* Parse TSO parms */
call parseArgs argstr
/* ------------ Globals ------------------------------------------- */
isInDDOpen  = 0      /* indicator to track if input file is open    */
isMapDDOpen = 0      /* indicator to track if map file is open      */
isLayDDOpen = 0      /* indicator to track if layout file is open   */
isOutDDOpen = 0      /* indicator to track if output file is open   */
isLayMemOpen= 0      /* indicator to track if output file is open   */
/* ------------ Globals Arrays -------------------------------------*/
maps.1 = ""
maps.0 = 0           /* mapping entries (exact)                     */
wilds.0 = 0          /* mapping entries (wildcards)                 */
layouts. = 0         /* per-member parsed layout schema             */
layout_loaded. = 0   /* loaded members                              */
loadedMembers.0 = 0  /* initializing the array count.               */
seenLoaded. = 0
ctVar. = 0           /* holds only -CT counters, default 0          */
ctVar_keys.0 = 0     /* ordered list of ctVar tails for display */
ctVar_seen. = 0      /* marks a tail as captured (dedupe)       */
firstObj = 1         /* to control commas between JSON objects      */
lastObject = ''
rec = ''             /* current record buffer                       */
mem = ''             /* current layout member                       */
jsonPairs = ''       /* current JSON key:value pairs                */
/* =========================== MAIN =============================== */
say '==============================================================='
say 'REC5JSON  : Starting ... '
say '==============================================================='
/* 1) Validate DDs, open streams, print dataset names, write header */
call validateAndOpenDDs
/* 2) Load Mapping and print the summary */
rcm = loadMapping_doUntil(mapDD)
/* Start reading the record and create JSON records */
if rcm <> 0 then do
  say 'ERROR: loadMapping_doUntil failed RC='rcm
  call closeAndFreeDDs 16
end

if maps.0 = 0 & wilds.0 = 0 then do
  say 'ERROR: No routing rules found in MAPFILE.'
  call closeAndFreeDDs 16
end
/* Pre-load layouts up front (optional but recommended) */
do i=1 to maps.0
   memx = maps.i.member
   if memx <> '' & layout_loaded.memx <> 1 then do
      call loadLayout layPDSDD, memx
   end
end

do i=1 to wilds.0
   memx = wilds.i.member
   if memx <> '' & layout_loaded.memx <> 1 then do
      call loadLayout layPDSDD, memx
   end
end
/* 3) (Optional) print a short summary of what we loaded */
rcs = summarizeMapping()
  if rcs <> 0 then do
    say 'WARNING: summarizeMapping returned RC='rcs
  end
/* 4) Print summaries (includes FILLER if SHOWFILLER(1)) */
/*    Uncomment the lines below to print layout in sysprint */
/*
call printAllLayouts 0, showFillerDefault /* loaded, with FILLER */
if summaryMember <> '' then do
   call printLayoutSummary summaryMember, 0, showFillerDefault
end
*/
/* 5) Write the wrapper JSON name in output file. */
call JsonLines_Init
/* TODO: Real record loop goes here. Once routing is wired, call:
         call initParseState  rec, memb
         call parseRecord     memb
         -- and write jsonPairs to OUTDD (1 JSON object per record)
*/
/* --------------------------- */
/* ------- RECORD LOOP ------- */
/* --------------------------- */
/* If INDD allocated, process records -> one JSON object per record */
if isInDDOpen = 1 then do
  do until rc <> 0
    Address TSO "EXECIO 1 DISKR "inDD" (STEM R."
    if rc <> 0 then leave

    rec = R.1
    key_v = strip(substr(rec, 37, 9))  /* Extract key from record */
    /*A_v = strip(substr(rec, 37, 3))   Extract key from record */
    memb = routeMember(key_v)     /* Get layout member from MAPFILE*/
    /*y 'key108: ' key_v */
    /*y 'memb108: ' memb */

    /* Route to layout member */
    if memb = '' then do
      /* No route; skip silently or log as needed */
      iterate
    end
    if pos('BA3', rec) > 0 then
      TBA = '3X'
    else
      TBA = '4x'
    jsonPairs = ''

    /* Ensure layout is loaded */
    if layout_loaded.memb <> 1 then do
      call loadLayout layPDSDD, memb
      if layout_loaded.memb <> 1 then iterate
    end

    /* Parse with chosen layout and produce jsonPairs */
/*  say 'Record read: ' rec */
    say 'Key extracted: ' key_v
    say 'layout_loaded.memb : ' layout_loaded.memb
    say 'Layout member: ' memb
    call initParseState rec, memb

    call parseRecord memb, TBA

    /* call displayCtVar */
    /* ---- Quick tests for getCtForGroup ----
    say 'CT(WS-CVCAT)        = ' getCtForGroup('WS-CVCAT')
    say 'CT(WS-PR)           = ' getCtForGroup('WS-PR')
    say 'CT(WS-SA)           = ' getCtForGroup('WS-SA')
    say 'CT(WS-SA-SRCRSLT)   = ' getCtForGroup('WS-SA-SRCRSLT')
    say 'CT(WS-SUGGXPLDGNS)  = ' getCtForGroup('WS-SUGGXPLDGNS')
       ------------------------------------------- */

    /* Write one JSON object */
    call Json_WriteObject jsonPairs

  end

end

else  /* isInDDOpen = 0 */
  say 'WARNING: INDD not allocated; only layout summary was produced.'

/* 6) Write the end of wrapper JSON in output file. */
call JsonLines_Close
/* 7) Footer, close and FREE */
call closeAndFreeDDs 0
say '==============================================================='
say 'REC2JSON  : Done.'
say '==============================================================='

exit 0
/********************************************************************/
/*  END OF PROGRAM */
/********************************************************************/
/* ====================== PROCEDURES ============================== */
/* PROCEDURE PARSEARGS */
parseArgs: procedure expose inDD mapDD layPDSDD outDD
  parse upper arg s

  do while s <> ''
    parse upper var s tok '('parm')' s
    select
      when tok = 'INDD'    then inDD  = parm
      when tok = 'MAPFILE' then mapDD = parm
      when tok = 'LAYLIB'  then layPDSDD = parm
      when tok = 'OUTDD'   then outDD = parm
      when tok = 'SHOWFILLER' then
        if datatype(parm,'N') then
          showFillerDefault = parm + 0
        else
          showFillerDefault = 1
      otherwise nop
    end
  end

return

/* --------- Procedure 0: Get Dataset name of DD --------------- */
getDSName: procedure
  parse arg dd
  rc = LISTDSI(dd || ' FILE')
  if rc = 0 then do
    dsname = SYSDSNAME
  end
  else dsname = '(UNKNOWN)'
return dsname

/* --------- Procedure 1: Validate & Open DDs + Header --------- */
validateAndOpenDDs : procedure expose inDD mapDD layPDSDD outDD firstObj isInDDOpen isOutDDOpen
  /* Resolve actual dataset names and basic attributes */
  ds_in = getDSName(inDD)
  ds_map = getDSName(mapDD)
  ds_lay = getDSName(layPDSDD)
  ds_out = getDSName(outDD)

  say 'INFILE  = ' ds_in
  say 'MAPFILE = ' ds_map
  say 'LAYLIB  = ' ds_lay
  say 'OUTFILE = ' ds_out

  /* Validate allocations via LISTDSI (REXX function) and attributes */
  /* INFILE must exist (typically PS, RECFM contains V) */
  if LISTDSI(inDD || ' FILE') <> 0 then do
    say 'ERROR: INFILE DD not allocated.'
    call closeAndFreeDDs 16
  end
  if pos('V', SYSRECFM) = 0 then do
    say 'WARNING: INFILE RECFM='SYSRECFM' (expected variable).'
  end

  /* MAPFILE - required */
  if LISTDSI(mapDD || ' FILE') <> 0 then do
    say 'ERROR: MAPFILE DD not allocated.'
    call closeAndFreeDDs 16
  end
  if SYSRECFM <> 'F' & SYSRECFM <> 'FB' then
    say 'WARNING: MAPFILE RECFM='SYSRECFM' (expected F/FB).'
  if DATATYPE(SYSLRECL,'W') then do
    l = SYSLRECL + 0
    if l <> 80 then say 'WARNING: MAPFILE LRECL='l' (expected 80).'
  end

  /* LAYLIB - required */
  if LISTDSI(layPDSDD || ' FILE') <> 0 then do
    say 'ERROR: LAYLIB DD not allocated.'
    call closeAndFreeDDs 16
  end
  if translate(SYSDSORG) <> 'PO' then do
    say 'ERROR: LAYLIB DSORG='SYSDSORG' (expected PO).'
    call closeAndFreeDDs 16
  end

  /* OUTDD - if allocated, open for write */
  if LISTDSI(outDD ' FILE') = 0 then do
    ADDRESS TSO "EXECIO 0 DISKW " outDD "(OPEN"
    if rc <> 0 then do
      say 'ERROR: cannot OPEN ' outDD
      call closeAndFreeDDs 16
    end
    else
      isOutDDOpen = 1
  end
  else do
    say 'WARNING: OUTDD not allocated. Allocating temporary SYSOUT.'
    "ALLOC FI("outDD") SYSOUT(*) RECFM(VB) LRECL(32756) BLKSIZE(32760) REUSE"
    if rc <> 0 then do
      say 'ERROR: dynamic ALLOC for ' outDD ' failed RC='rc
      call closeAndFreeDDs 16
    end
    ADDRESS TSO "EXECIO 0 DISKW" outDD "(OPEN"
    if rc <> 0 then do
      say 'ERROR: cannot OPEN dynamically allocated ' outDD
      call closeAndFreeDDs 16
    end
    else
      isOutDDOpen = 1
  end

  /* INDD - optional processing */
  ADDRESS TSO "EXECIO 0 DISKR "inDD" (OPEN"
  if rc <> 0 then do
    say 'ERROR: cannot OPEN ' inDD
    call closeAndFreeDDs 16
  end
  else
    isInDDOpen = 1
return

/* --------- Procedure 2: Footer, Close & FREE DDs --------- */
closeAndFreeDDs: procedure expose inDD outDD mapDD layPDSDD isOutDDOpen isInDDOpen isMapDDOpen isLayDDOpen isLayMemOpen
  parse arg rcWanted

  /* Normalize rcWanted: empty means 0; non-numeric treated as 0 */
  if rcWanted = '' then rcWanted = 0
  else if \datatype(rcWanted,'N') then rcWanted = 0
  else rcWanted = rcWanted + 0

  /* If input file is open, then close it and free up memory */
  if isOutDDOpen = 1 then do
    ADDRESS TSO "EXECIO 0 DISKW "outDD" (FINIS"
    ADDRESS TSO "FREE FI("outDD")"
    isOutDDOpen = 0
  end

  /* if output file is open, then close it and free up memory */
  if isInDDOpen = 1 then do
    ADDRESS TSO "EXECIO 0 DISKR "inDD"  (FINIS"
    ADDRESS TSO "FREE FI("inDD")"
    isInDDOpen = 0
  end

  /* if map library file is found open, then close and free it */
  if isMapDDOpen = 1 then do
    ADDRESS TSO "FREE FI("mapDD")"
    isMapDDOpen = 0
  end

  /* if layout library file is found open, then close and free it */
  if isLayDDOpen = 1 then do
    ADDRESS TSO "FREE FI("layPDSDD")"
    isLayDDOpen = 0
  end

  /* if layout member file is found open, then close and free it */
  if isLayMemOpen = 1 then do
    ADDRESS TSO "FREE FI(LAYMEM)"   /* case any leftover member alloc */
    isLayMemOpen = 0
  end

  if rcWanted <> 0 then
     exit rcWanted
return

/* ----------  Procedure : Write the Wrapper JSON Array -------- */
JsonLines_Init : procedure expose jsonLine. outDD
  drop jsonLine.
  say 'Writing JSON.'
  jsonLine.0 = 1
  jsonLine.1 = '{ "dataAssignments" : [ '
  Address Tso "EXECIO "jsonLine.0" DISKW "outDD" (STEM jsonLine."
  isOutDDOpen = 1
  if rc <> 0 then do
    say 'ERROR: EXECIO DISKW STEM failed RC='rc
    ADDRESS TSO "EXECIO 0 DISKW "outDD" (FINIS"
    return rc
  end
return

/* ---------- Procedure : Write JSON Object into dataset --------- */
Json_WriteObject: procedure expose outDD firstObj lastObject
  parse arg objPairs
  drop L.
  if firstObj = 1 then do
    L.0 = 1
    L.1 = '{ ' || strip(objPairs, 'L', ',') || ' }'
    firstObj = 0
    lastObject = L.1
  end
  else do
    L.0 = 2
    L.1 = ','
    L.2 = '{ ' || strip(objPairs, 'L', ',') || ' }'
    lastObject = L.2
  end
  Address TSO "EXECIO" L.0 "DISKW" outDD "(STEM L."
return

/* Call when all records are processed */
JsonLines_Close: procedure expose jsonLine. outDD
  drop L.
  L.0 = 1
  L.1 = ']}'
  Address TSO "EXECIO " L.0 " DISKW " outDD " (STEM L."
  isOutDDOpen = 1
  if rc <> 0 then do
    say 'ERROR: EXECIO DISKW STEM failed RC='rc
    ADDRESS TSO "EXECIO 0 DISKW "outDD" (FINIS"
    return rc
  end
return

/* ===================== Mapping Loader =========================== */
/* Reads MAPFILE (fixed 80). Lines beginning with 'ct' are ignored. */
/* Key  : cols  1- 9 */
/* Member: cols 40-47 */
/* Exact keys -> maps.; wildcard keys (contain *) -> wilds.         */
loadMapping_doUntil: procedure expose maps. wilds. isMapDDOpen
  parse arg dd
  total   = 0 ; skipped = 0

  /* Open MAPFILE for read (this proc manages OPEN/FINIS itself) */
  Address Tso "EXECIO 0 DISKR "dd" (OPEN"
  if rc <> 0 then do
    say 'ERROR: cannot OPEN ' dd ' for read. RC='rc
    return 16
  end
  isMapDDOpen = 1

  /* Safe DO UNTIL end-of-file (rc<>0 after EXECIO) */
  do until rc <> 0
    Address Tso "EXECIO 1 DISKR "dd" (STEM L."
    if rc <> 0 then iterate
    ln = strip(L.1,'T')
    total = total + 1

    /* Trim right; keep left (pos-based columns are still valid) */
    /* ignore comments and blank/short lines */
    cleanU = translate(ln)
    if cleanU = ''                           then iterate
    if left(ln,2) = '/*'                     then iterate
    if length(ln) < 47   then do
      skipped = skipped + 1
      iterate
    end

    keytx  = strip(substr(ln,1,9))
    memb = strip(substr(ln,40,8))
/*  say 'keytx = ' keytx
    say 'memb = ' memb */
    if keytx = '' | memb = '' then do
      skipped = skipped + 1
      iterate
    end

    if pos('****', keytx) > 0 then do
      w = wilds.0 + 1
      wilds.w.key    = keytx
      wilds.w.member = memb
      wilds.0 = w
    end
    else do
      x = maps.0 + 1
      maps.x.key    = keytx
      maps.x.member = memb
      maps.0 = x
      /*y 'maps.x.key: ' maps.x.key */
    end

  end

  /* Close and free */
  "EXECIO 0 DISKR "dd" (FINIS"
  "FREE FI("dd")"
  isMapDDOpen = 0
  say '--- MAP LOAD --------------------------------------------------'
  say '  Read    : ' total 'line(s)'
  say '  Skipped : ' skipped
  say '  Loaded  : exact='maps.0', wildcard='wilds.0
return 0

/* Optional: quick summary of a few entries */
summarizeMapping: procedure expose maps. wilds.
  say '--- SUMMARY (first few entries) -------------------------------'
  say 'Exact routes   : ' maps.0
  if maps.0 > 0 then do
    limit = maps.0
    do i = 1 to limit
      say '  ' right(i,2) ') ' maps.i.key ' -> ' maps.i.member
    end
  end
  else do
    say '  (none)'
  end
  say 'Wildcard routes: ' wilds.0
  if wilds.0 > 0 then do
    limit = wilds.0
    do i = 1 to limit
      say '  ' right(i,2) ') ' wilds.i.key ' -> ' wilds.i.member
    end
  end
  else do
    say '  (none)'
  end
return 0

/* Small helper: MIN() because REXX lacks built-in */
min : procedure
  parse arg a,b
  if a < b then return a
  return b

/* Simple router (exact then simple wildcards) */
routeMember : procedure expose maps. wilds.
  parse arg key_v

  /*y 'maps.0465: ' maps.0 */
  do i = 1 to maps.0
    /*y 'maps.i.key465: ' maps.i.key */
    if key_v = maps.i.key then
      return maps.i.member
  end

  if wilds.0 > 0 then do
     return wilds.1.member
  end

/* naive wildcard: supports 'ABC*', '*XYZ', 'A*Z' */
/*
  do i = 1 to wilds.0
    pat = wilds.i.key
    if right(pat,1)='*' then do
      pre = substr(pat,1,length(pat)-1)
      if left(key,length(pre))=pre then return wilds.i.member
    end
    else if left(pat,1)='*' then do
      suf = substr(pat,2)
      if right(key,length(suf))=suf then return wilds.i.member
    end
    else if pos('*',pat)>0 then do
      parse var pat pre '*' suf
      if left(key,length(pre))=pre & right(key,length(suf))=suf then
        return wilds.i.member
    end
  end
*/
  return ''

/* ---- Load one copybook member --------------------------------- */
loadLayout: procedure expose layouts. layout_loaded. isLayMemOpen seenLoaded. loadedMembers.
  parse arg layPDSDD, member

  say 'Loading layout of member name: ' member
/* Resolve DSN behind the DD (e.g., LAYLIB) */
  rc = LISTDSI(layPDSDD || ' FILE')

  if rc <> 0 then do
    say 'ERROR: DD' layPDSDD 'not allocated or not accessible. RC='rc
    say ' MSG1='SYSMSGLVL1
    say ' MSG2='SYSMSGLVL2
    return 16
  end

  dsn = SYSDSNAME
/*say 'Resolved LAYLIB DSN = ' dsn */
/* Free any lingering FI from a prior call (ignore if not allocated*/
  if isLayMemOpen = 1 then do
    address TSO "FREE FI(LAYMEM)"
    isLayMemOpen = 0
  end

/* Build a safe ALLOC command: DA('dsn(member)') */
  cmd = "ALLOC FI(LAYMEM) DA('" || dsn || "(" || member || ")') SHR REUSE"
/*say 'DEBUG ALLOC CMD: ' cmd */
  address TSO cmd
  if rc <> 0 then do
    say 'ERROR: cannot ALLOC ' pds '(' member ') RC='rc
    call closeAndFreeDDs 16
  end

  ADDRESS TSO "EXECIO * DISKR LAYMEM (STEM L. FINIS"
  isLayMemOpen = 1
  if rc <> 0 then do
    say 'ERROR: reading layout 'member' RC='rc
    Address TSO "FREE FI(LAYMEM)"
    isLayMemOpen = 0
    call closeAndFreeDDs 16
  end

/* Parse and free */
  call parseCopybook member
  address TSO "FREE FI(LAYMEM)"
  isLayMemOpen = 0
  layout_loaded.member = 1

/* add to numeric list once */
  if seenLoaded.member <> 1 then do
    n = loadedMembers.0 + 1
    loadedMembers.n = member
    loadedMembers.0 = n
    seenLoaded.member = 1
  end
return 0

/* ---- Parse copybook content into a schema tree ----------------- */
parseCopybook: procedure expose layouts. L.
  parse arg member
  /* Copy raw lines */
  drop CB.
  CB.0 = L.0
  do i=1 to L.0
    CB.i = L.i
  end
  /* Normalize & join statements */
  drop SRC.
  SRC.0 = 0
  hold   = ''
  name_v = ''
  do i=1 to CB.0
    line = CB.i
    if length(line) >= 72 then do
      ind  = substr(line,7,1)       /* column 7: indicator */
      area = substr(line,8,65)      /* columns 8-72: Area A/B */
    end
    else do
      ind  = substr(line,7,1)       /* column 7: indicator */
      area = line                   /* columns 8-72: Area A/B */
    end
    /* Skip pure comment lines via indicator column */
    if ind = '*' then iterate /* pure comment line */
    area = strip(area)
    p = pos('*>', area)             /* remove trailing inline remarks */
      if p > 0 then area = strip(substr(area,1,p-1))
    area = space(area, 1)
    if area = '' then iterate
    /* NEW: normalize whitespace to a single space */
    /* accumulate until terminating '.' */
    /* NEW: join logic with continuation awareness */
    if ind = '-' then do
      /* hard continuation: append directly, no extra blank */
      hold = strip(hold || area)
    end
    else do
      /* normal: keep a single blank between tokens */
      hold = strip(hold || ' ' || area)
    end
    hold = strip(hold || ' ' || area)
    if right(hold,1) = '.' then do
      s = strip(strip(hold,'T','.'),'B')
      c = src.0 + 1
      src.c = s
      src.0 = c
      hold  = ''
    end
  end
  /* Flush dangling text (no trailing '.') */
  if hold <> '' then do
    c = src.0 + 1
    src.c = strip(strip(hold,'T','.'),'B')
    src.0 = c
  end

  /* Build temporary node table T. */
  drop T. levelStack.
  T.0 = 0

  do i=1 to src.0
    s = translate(src.i)
    parse var s lvl name_v rest
    if datatype(lvl)<> 'NUM' then iterate
    if name_v = '' | datatype(name_v,'N') then iterate

    lvl = lvl + 0
    if pos(' REDEFINES ', rest)>0 then iterate

    /*cursMin=0; occursMax=0; depOn_v = '' */
    occursMin=1; occursMax=1; depOn_v = ''

    /* OCCURS (and optional TO ... DEPENDING ON ...) */
    if pos(' OCCURS ', rest) > 0 then do
      parse var rest before ' OCCURS ' after
      rest = before
      parse var after n after2
      n = strip(n,'T','.')
      if datatype(n)='NUM' then occursMin = n+0
        occursMax = occursMin
      if pos(' DEPENDING ON ', after2) > 0 then do
        at2 = after2
        if pos(' TO ', at2) > 0 then do
          parse var at2 ' TO ' m ' DEPENDING ON ' depOn_v .
          m = strip(m,'T','.')
          if datatype(m)='NUM' then
            occursMax = m+0
          depOn_v = space(depOn_v,0)
        end
        else do
          parse var at2 ' DEPENDING ON ' depOn_v .
          depOn_v = space(depOn_v,0)
        end
      end
    end

    /* PIC / PICTURE + USAGE */
    temp = ' 'rest' '
    /* Correct: detect PIC/PICTURE with OR */
    hasPIC = (pos(' PIC ', temp) > 0) | (pos(' PICTURE ', temp) > 0)

    /* Extract PIC (either spelling) */
    usage_v   = 'DISPLAY'
    pictype_v = 'ALPHA'
    digit_v   = 0
    decs_v    = 0
    bytes_v   = 0
    pic_v     = ''

    /*  temp = ' '||rest||' ' */
    if pos(' PIC ', temp) > 0 then do
      parse var temp . ' PIC ' piccl .
      pic_v = strip(piccl, 'B', '.')     /* both ends: blanks + dots */
      call parsePIC pic_v, digit_v, decs_v, pictype_v
    /*say 'pic = ' pic_v
      say 'digit_s = ' digit_v
      say 'decs ='decs_v
      say 'pictype = ' pictype_v */
    end
    else if pos(' PICTURE ', temp) > 0 then do
      parse var temp . ' PICTURE ' piccl .
      pic_v = strip(piccl, 'B', '.')     /* both ends: blanks + dots */
      call parsePIC pic_v, digit_v, decs_v, pictype_v
    /*say 'pic = ' pic_v
      say 'digit_s = ' digit_v
      say 'decs ='decs_v
      say 'pictype = ' pictype_v */
    end


    /* USAGE (explicit) */
    if pos(' USAGE ', temp) > 0 then do
      parse var temp . ' USAGE ' usageW .
      usageW = strip(usageW, 'B', ' .')
      select
        when usageW = 'COMP-3' | usageW = 'PACKED-DECIMAL' then
          usage_v = 'COMP-3'
        when usageW = 'COMP'   | usageW = 'BINARY'         then
          usage_v = 'COMP'
        when usageW = 'POINTER' then
          usage_v = 'POINTER'
        otherwise usage_v='DISPLAY'
      end /* end select */
    end
    /* detect bare COMP/COMP-3/BINARY/POINTER (no USAGE keyword) */
    else do
      if pos(' COMP-3 ', temp) > 0 | pos(' PACKED-DECIMAL ', temp) > 0 then
        usage_v = 'COMP-3'
      else if pos(' COMP ', temp) > 0 | pos(' BINARY ', temp) > 0 then
        usage_v = 'COMP'
      else if pos(' POINTER ', temp) > 0 then
        usage_v = 'POINTER'
      else usage_v = 'DISPLAY'
    end
    /*say 'Usage496: ' usage */
    /* Compute bytes where possible */
    if hasPIC then do
      select
        when pictype_v = 'ALPHA' then
          bytes_v = digit_v
        when pictype_v = 'NUM' & usage_v = 'DISPLAY' then
          bytes_v = digit_v + decs_v
        when usage_v = 'COMP-3' then do
        /* packed bytes_v = ceil((digits+decs+1)/2) */
          totNibbles = digit_v + decs_v + 1
          bytes_v = (totNibbles + 1) % 2 /* integer division -> ceil */
        end
        when usage_v = 'COMP' then do
          tot = digit_v + decs_v
          if tot <= 4 then bytes_v=2
          else if tot <= 9 then bytes_v=4
          else bytes_v=8
        end
        otherwise bytes_v = digit_v + decs_v
      end /* end select */
    end /* end hasPIC condition */
/* POINTER without PIC uses platform size; LP(32) -> 4 bytes */
    if usage_v = 'POINTER' then do
      bytes_v = 4
      hasPIC = 0
      pictype_v = 'NUM'
    end

/* USAGE POINTER without PIC -> leave bytes as 0 unless
   you fix platform size */
    k = T.0 + 1
    T.k.level       = lvl
    T.k.name        = name_v
    T.k.hasPIC      = hasPIC
    T.k.pictype     = pictype_v
    T.k.usage       = usage_v
    T.k.digit_s     = digit_v
    T.k.decs        = decs_v
    T.k.bytes       = bytes_v
    T.k.occMin      = occursMin
    T.k.occMax      = occursMax
    T.k.depOn       = depOn_v
    T.k.parent      = 0
    T.k.children.0  = 0
    if usage_v = 'POINTER' then T.k.isGroup = 0
    else T.k.isGroup = (hasPIC=0)

  /* parent by nearest lower level on stack */
    p = 0
    do ll = lvl-1 to 1 by -1
      if levelStack.ll <> '' then do
        p = levelStack.ll
        leave
      end
    end
    if p > 0 then do
      T.k.parent = p
      c = T.p.children.0 + 1
      T.p.children.c = k
      T.p.children.0 = c
    end

    levelStack.lvl = k
    do ll = lvl+1 to 49
      levelStack.ll = ''
    end

    T.0 = k


  end  /* End of do i=1 to src.0 */

  /* Copy to layouts.member.* */
  do i = 1 to T.0                     /* <-- start at 1, not 0 */
    /* Get child count safely (default 0 if not set or non-numeric) */
    ch0 = 0
    if symbol('T.'i'.children.0') = 'VAR' then do
      ch0 = T.i.children.0
      if \datatype(ch0,'N') then ch0 = 0
    end
    /* Copy scalar attributes */
    layouts.member.i.level   = T.i.level
    layouts.member.i.name    = T.i.name
    layouts.member.i.hasPIC  = T.i.hasPIC
    layouts.member.i.pictype = T.i.pictype
    layouts.member.i.usage   = T.i.usage
    layouts.member.i.digit_s = T.i.digit_s
    layouts.member.i.decs    = T.i.decs
    layouts.member.i.bytes   = T.i.bytes
    layouts.member.i.occMin  = T.i.occMin
    layouts.member.i.occMax  = T.i.occMax
    layouts.member.i.depOn   = T.i.depOn
    layouts.member.i.parent  = T.i.parent
    layouts.member.i.isGroup = T.i.isGroup
    layouts.member.i.children.0 = ch0

    do j=1 to ch0
      layouts.member.i.children.j = T.i.children.j
    end
    /* Debugging Lines
    say layouts.member.i.level
    say layouts.member.i.name
    say layouts.member.i.hasPIC
    say layouts.member.i.pictype
    say layouts.member.i.usage
    say layouts.member.i.digit_s
    say layouts.member.i.decs
    say layouts.member.i.bytes
    say layouts.member.i.occMin
    say layouts.member.i.occMax
    say layouts.member.i.depOn
    say layouts.member.i.parent
    say layouts.member.i.isGroup
    say layouts.member.i.children.0 */
  end

  layouts.member.topCount = 0
  do i=1 to T.0
    if T.i.parent=0 then do
      c = layouts.member.topCount + 1
      layouts.member.top.c = i
      layouts.member.topCount = c
    end
  end
return

/* ---- Parse PIC string into (digit_s, decs, pictype) ------------ */
parsePIC: procedure expose digit_v decs_v pictype_v
  parse arg pic_v, digit_v, decs_v, pictype_v
  digit_v=0; decs_v=0; pictype_v='ALPHA'

  p = translate(changestr(' ', pic_v, ''))
  if left(p,1)='X' then do
    pictype_v='ALPHA'
    if pos('X(', p)>0 then parse var p 'X(' n ')' .
    else n=1
    if datatype(n)='NUM' then digit_v=n+0
    else digit_v = 1
  end
  else do
    pictype_v ='NUM'
    s = p
    if left(s,1)='S' then s=substr(s,2)
    vpos = pos('V', s)
    if vpos=0 then do
      digit_v = picCount9(s)
      decs_v = 0
    end
    else do
      iPart = substr(s,1,vpos-1)
      dPart = substr(s,vpos+1)
      digit_v = picCount9(iPart)
      decs_v   = picCount9(dPart)
    end
  end
return

/* -------- Picture Counts digits ------------ */
picCount9: procedure
  parse arg s
  if pos('9(', s)>0 then do
    parse var s '9(' n ')' .
    if datatype(n)='NUM' then return n+0
    else return 1
  end
  else do
    cnt = 0
    do i=1 to length(s)
      if substr(s,i,1)='9' then cnt=cnt+1
    end
    if cnt=0 then cnt=1
    return cnt
  end

/* ---- Utility: replace all occurrences -------------------------- */
changestr: procedure
  parse arg find, str, repl
  out=''; posn=1; fl = length(find)
  if fl=0 then return str
  do forever
    k = pos(find, str, posn)
    if k=0 then do
      out = out || substr(str,posn)
      leave
    end
    out = out || substr(str,posn,k-posn) || repl
    posn = k + fl
  end
return out

/* ========= Record Parsing -> jsonParis =============== */
initParseState: procedure expose rec mem jsonPairs ctVar. ctVar_keys. ctVar_seen.
  parse arg rec, mem
  jsonPairs = ''    /* start fresh per record; we will add fields */
  drop ctVar. ctVar_keys. ctVar_seen.
  ctVar. = 0
  ctVar_keys.0 = 0
return

parseRecord: procedure expose layouts. jsonPairs recOff rec depval. ctVar. ctVar_keys. ctVar_seen.
  parse arg member, TBA
/*********** PASS 1 -- scan all fields to capture -CT values ****/
  recOff = 1
  do ti=1 to layouts.member.topCount
    node = layouts.member.top.ti
    call parseNode member, node, '', TBA, 'SCANCT'
  end

/*********** PASS 2 -- real emission guided by captured counts **/
  recOff = 1
  do ti=1 to layouts.member.topCount
    node = layouts.member.top.ti
    call parseNode member, node, '', TBA, 'NORMAL'
  end
return

parseNode: procedure expose layouts. jsonPairs recOff rec ctVar. ctVar_keys. ctVar_seen.
  parse arg member, idx, path, TBA, mode
  if mode = '' then mode = 'NORMAL'

  isGroup  = layouts.member.idx.isGroup
  name     = layouts.member.idx.name
  depOn    = layouts.member.idx.depOn
  occMin   = layouts.member.idx.occMin
  occMax   = layouts.member.idx.occMax

  fullName = name
  if path <> '' then fullName = path || '.' || name

  /* Effective OCCURS for parsing */
  occ = occMin

  if depOn <> '' then do
    val = getCtForFieldOrGroup(depOn, name)
    if datatype(val) = 'NUM' then occ = val
    if occ > occMax then occ = occMax
    if occ < occMin then occ = occMin
  end

  /* For sibling-CT convention (e.g. WS-CVCAT-CT) */

  /* -------- GROUP NODE -------- */
  if isGroup then do

    ctName  = translate(name)
    ctEmit  = getCtForGroup(ctName)
    wantArray      = (occ > 1)
    arrCountToEmit = min(occ, ctEmit)

    /* While emitting (NORMAL), use the per-element sibling CT */
    if mode = 'NORMAL' then do     /* NORMAL Mode */
      if datatype(ctEmit,'N') & ctEmit > 0 then do
        arrCountToEmit = min(occ, ctEmit + 0)
      end
      else do
        ctFromCtVar = getCtForGroup(name)
        if \datatype(ctFromCtVar,'N') then ctFromCtVar = 0
        if ctFromCtVar < 0 then ctFromCtVar = 0
        arrCountToEmit = min(occ, ctFromCtVar)
      end
    end
    else do                        /* SCAN Mode */
    /* OCCURS already computed into 'occ' above */
      ctFromCtVar = getCtForGroup(name)
      if \datatype(ctFromCtVar,'N') then ctFromCtVar = 0
      if ctFromCtVar < 0 then ctFromCtVar = 0
      arrCountToEmit = min(occ, ctFromCtVar)
    end

    say 'DBG mode = ' mode ' group=' name'-CT occ=' occ ' ctEmit=' ctEmit ' emit=' arrCountToEmit

    /* === SINGLE (no array) === */
    if wantArray = 0 then do
      newPath = path
      do ci=1 to layouts.member.idx.children.0
        ch = layouts.member.idx.children.ci
        call parseNode member, ch, newPath, TBA, mode
      end
    end
    else do
      /* === ARRAY path ===
        Parse ALL physical OCCURS to keep offsets aligned,
        but emit only first arrCountToEmit elements. */
      arrBuf = ''
      do ii = 1 to occ
        oldJP   = jsonPairs
        jsonPairs = ''
        newPath = path
        do ci=1 to layouts.member.idx.children.0
          ch = layouts.member.idx.children.ci
          call parseNode member, ch, newPath, TBA, mode
        end
        oneObj = '{ ' || strip(jsonPairs, 'L', ',') || ' }'

/* ********** Emit strictly based on CT (or OCCURS if no CT) *********/
        if ii <= arrCountToEmit then do
          if arrBuf = '' then arrBuf = oneObj
          else arrBuf = arrBuf || ',' || oneObj
        end

        jsonPairs = oldJP
      end

/* Add the array property (empty [] if count=0) */
      keyq = jsonQuote(name)
      if mode = 'NORMAL' then
        jsonPairs = jsonPairs || ',' || keyq || ' : [ ' || arrBuf || ' ]'
    /* If you prefer to OMIT the property when CT=0, replace with:
      if mode='NORMAL' & arrCountToEmit>0 then
        jsonPairs = jsonPairs || ',' || keyq || ' : [ ' || arrBuf || ' ]'
    */
    end
  end
   /* -------------------- END GROUP NODE -------------------- */
  else do /* Not a group */
   /* -------- SCALAR NODE -------- */
    bytes   = layouts.member.idx.bytes
    pictype = layouts.member.idx.pictype
    usage   = layouts.member.idx.usage
    digit_s = layouts.member.idx.digit_s
    decs    = layouts.member.idx.decs

    if recOff + bytes - 1 > length(rec) then
      raw = copies(' ', bytes)
    else
      raw = substr(rec,recOff,bytes)

    /*c=1   */
    if occ=1 then do
      if mode <> 'SCANCT' then do
      say 'Field:' left(fullName,25) ' Start=' right(recOff,6) ' Len=' right(bytes,4) ' Hex=' c2x(raw)
      end

      val = parseScalar(raw, pictype, usage, digit_s, decs)
      recOff = recOff + bytes

      /* Force WS-PR-CT to 2 for layout member C4391500A */
      if translate(name) = 'WS-PR-CT' & pos('SG4391', translate(member)) > 0 then do
        val = 2
      end

      call maybeRemember name, fullName, val, pictype, usage

      /* During SCAN pass, store only -CT fields into ctVar. */
      if mode = 'SCANCT' then do
        if right(translate(name), 3) = '-CT' then do
          ctTail = safeTail(name)  /* e.g., WS-CVCAT-CT -> WS_CVCAT_CT */

          if datatype(val,'N') then newCt = val + 0
          else do
            tnum = onlyDigits(val) /* defensive: ensure numeric */
            if tnum = '' then tnum = '0'
            newCt = tnum + 0
          end

          /* keep the maximum seen for this -CT in SCAN pass */
          prevCt = 0
          if symbol('ctVar.'ctTail) = 'VAR' then do
            if datatype(ctVar.ctTail,'N') then
              prevCt = ctVar.ctTail + 0
          end
          if newCt > prevCt then ctVar.ctTail = newCt

          if symbol('ctVar_keys.0') <> 'VAR' then ctVar_keys.0 = 0
          else if \datatype(ctVar_keys.0,'N') then ctVar_keys.0 = 0
          if ctVar_seen.ctTail <> 1 then do
            n = ctVar_keys.0 + 1
            ctVar_keys.n = ctTail
            ctVar_keys.0 = n
            ctVar_seen.ctTail = 1
          end
        end
      end

 /*********** NEW: Only emit JSON in NORMAL pass ********** */
      if mode = 'NORMAL' then do
      /* Skip FILLER in JSON */
        if translate(name) = 'FILLER' then do
           fullName = 'CICS'
           val_c = substr(val,1,4)
           call addPair fullName, val_c, pictype, usage
           fullName = 'TBA'
           val_s = TBA
           call addPair fullName, val_s, pictype, usage
           fullName = 'EIBTASKN'
           val_task = substr(val,31,4)
           val_task = c2x(val_task)
           val_task = substr(val_task, 1, length(val_task) - 1)
           call addPair fullName, val_task, pictype, usage
        end
        else do
           call addPair fullName, val, pictype, usage
        end
      end
    end       /* occ = 1 */

    else do   /* occ <> 1 */
      do ii=1 to occ
        if recOff + bytes - 1 > length(rec) then do
           raw = copies(' ', bytes)
        end
        else do
          raw = substr(rec,recOff,bytes)
        end
        say 'Field=' left(fullName,25) ' Start=' right(recOff,6) ' Len=' right(bytes,4) ' Hex=' c2x(raw)
        val = parseScalar(raw, pictype, usage, digit_s, decs)
        recOff = recOff + bytes

       /* your special override for WS-PR-CT, keep if needed */
        if translate(name) = 'WS-PR-CT' & pos('SG4391', translate(member)) > 0 then do
            val = 2
        end

        call maybeRemember name, fullName||'['ii']', val, pictype, usage

        if mode = 'SCANCT' then do
          if right(translate(name), 3) = '-CT' then do
            ctTail = safeTail(name)

            if datatype(val,'N') then newCt = val + 0
            else do
              tnum = onlyDigits(val)
              if tnum = '' then tnum = '0'
              newCt = tnum + 0
            end
            prevCt = 0
            if symbol('ctVar.'ctTail) = 'VAR' then do
              if datatype(ctVar.ctTail,'N') then prevCt = ctVar.ctTail + 0
            end

            if newCt > prevCt then ctVar.ctTail = newCt

        /* portable guard: ensure ctVar_keys.0 exists and is numeric */
            if symbol('ctVar_keys.0') <> 'VAR' then ctVar_keys.0 = 0
            else if \datatype(ctVar_keys.0,'N') then ctVar_keys.0 = 0
            if ctVar_seen.ctTail <> 1 then do
              n = ctVar_keys.0 + 1
              ctVar_keys.n = ctTail
              ctVar_keys.0 = n
              ctVar_seen.ctTail = 1
            end
          end
        end

        if mode = 'NORMAL' then do
        /* Skip FILLER in JSON */
          if translate(name)<> 'FILLER' then do
              call addPair fullName||'['ii']', val, pictype, usage
          end
        end
      end
    end  /* occ = 1 */
  end /* isGroup = True */

/*say 'Parsing field:' fullName 'Bytes='bytes 'Raw='raw */
return

parseScalar: procedure
  parse arg raw, pictype, usage, digit_s, decs

  select

    when usage = 'COMP-3' then do
      hx = c2x(raw)
      val = unpackComp3(hx, digit_s, decs)
      val = strip(val, 'L', '0')
      if substr(val, 1, 1) = '.' then val = '0' || val
      if pos('.', val) = 0 then val = val || '.00'
      /* Limit trailing zeros to exactly two */
      if pos('.', val) > 0 then do
        intp = substr(val, 1, pos('.', val) - 1)
        frac = substr(val, pos('.', val) + 1)

        /* If fraction is all zeros, force .00 */
        if verify(frac, '0') = 0 then
          val = intp || '.00'
      end
      return val
    end

    when usage = 'POINTER' then do
      return c2x(raw)
    end

    when usage = 'COMP' then do
      val = binToDec(raw)
      if decs > 0 then do
        if decs > 0 then
          return formatDecimal(val, decs)
      end
      return val
    end

    otherwise do
      s = strip(raw, 'T')

      if pictype='NUM' then do
        if decs > 0 then do
          s = onlyDigits(s)
          if s='' then s='0'
          if length(s)<=decs then s = copies('0', decs - length(s) + 1) || s
          intp = substr(s,1, length(s)-decs)
          frac = substr(s, length(s)-decs+1)
          if intp = '' then intp='0'
          return intp||'.'||frac
        end

        else do
          s = onlyDigits(s)
          if s = '' then s='0'
          return s+0
        end
      end
      else return s
    end
  end /* End-Select */

/* ----------- Unpacked Computation 3 values ------------- */
unpackComp3: procedure
  parse arg hex, digit_s, decs
  ln = length(hex)
  if ln = 0 then return 0
/*if pos('40', hex) > 0 then return '0.00' */
  body = substr(hex,1,ln-1)
  signNib = substr(hex,ln,1)
  num = ''
  do i=1 to length(body)
    nib = substr(body,i,1)
    /* Accept 0-9; coerce others to 0 */
    if x2d(nib) >= 0 & x2d(nib) <= 9 then num = num || nib
    else num = num || '0'
  end
  if num='' then num='0'
  if decs > 0 then do
    if length(num) <= decs then num = copies('0', decs - length(num)+1)
    intp = substr(num,1, length(num)-decs)
    frac = substr(num, length(num)-decs+1)
    if intp='' then intp='0'
    out = intp||'.'||frac
  end

  else out = num
  if signNib='D' | signNib='B' then out = '-'||out
return out

/* Convert binary value to decimal (signed, big-endian) */
binToDec: procedure
  parse arg raw
  len = length(raw)
return c2d(raw, len)

/* Format any decimal value */
formatDecimal: procedure
  parse arg val, decs
  abs = val; neg = (val<0)
  if neg then abs = -val
  str = abs||''
  if length(str)<=decs then str = copies('0', decs - length(str) + 1) ||
  intp = substr(str,1, length(str)-decs)
  frac = substr(str, length(str)-decs+1)
  out = intp||'.'||frac
  if neg then out = '-'||out
return out

onlyDigits: procedure
  parse arg s
  out=''
  do i=1 to length(s)
    ch = substr(s,i,1)
    if ch>='0' & ch<='9' then out=out||ch
  end
return out

depLookup: procedure
  parse arg dep., name
  nameU = safeTail(name)
  if dep.short.nameU <> '' then return dep.short.nameU
  if dep.full.nameU  <> '' then return dep.full.nameU
  /* fallback: scan short. */
  do k over dep.short.
    if k = nameU then
      return dep.short.k
  end
return 0
             save
maybeRemember: procedure
  parse arg short, full, val, pictype, usage
/*if datatype(val,'N') | pos('.',val)>0 | left(val,1)='-' then do
    shortU = safeTail(short)
    fullU  = safeTail(full)
    dep.short.shortU = val
    dep.full.fullU   = val
  end */
return

/* sanitize a name for use as a REXX stem tail */
safeTail: procedure
  parse arg s
  u = translate(s)
  out = ''
  do i = 1 to length(u)
    ch = substr(u,i,1)
    if (ch >= 'A' & ch <= 'Z') | (ch >= '0' & ch <= '9') | ch = '_' then
      out = out || ch
    else
      out = out || '_'
  end
return out


jsonQuote: procedure
  parse arg s
  s = changestr('\', s, '\\')
  s = changestr('"', s, '\"')
  allSpaces = 1
  do i = 1 to length(s)
    c = substr(s, i, 1)
    if c >= ' ' then do
      allSpaces = 0
      leave
    end
  end
  out=''
  if allSpaces = 1 then do
    out = ' '                      /* Collapse to single space */
  end
  else do
  /* Normal processing */
    do i=1 to length(s)
      c = substr(s,i,1)
     /*f c < ' ' then out=out||'?' */
      if c < ' ' then out=out||' '
      else out=out||c
    end
  end
return '"'||out||'"'

addPair: procedure expose jsonPairs
  parse arg key, val, pictype, usage
  keyq = jsonQuote(key)
  /*wline = d2c(10)  */
  if pictype='NUM' | usage='COMP' | usage='COMP-3' then do
    if datatype(val)='NUM' | pos('.',val)>0 | left(val,1)='-' then
       v = val
    else
       v = jsonQuote(val)
  end
  else
    v = jsonQuote(val)
  jsonPairs = jsonPairs || ',' || keyq || ':' || v
return

/* ============= Layout Summary Printing ========================== */
printLayoutSummary: procedure expose layouts. layout_loaded. lineCnt
  parse upper arg member maxLines showFiller

  /* Defaults */
  if maxLines   = '' then maxLines   = 0

  if showFiller = '' then showFiller = 0

  /* Basic checks */
  if member = '' then do
    say 'printLayoutSummary: member name not provided.'
    return 8
  end

  if layout_loaded.member <> 1 then do
    say 'printLayoutSummary: layout for member' member 'is not loaded.'
    return 8
  end

  if symbol('layouts.'member'.topCount') <> 'VAR' then do
    say 'printLayoutSummary: schema not found for member' member
    return 8
  end

  /* Header */
  say '---------------------------------------------------------------'
  say 'LAYOUT SUMMARY: ' member
  say '---------------------------------------------------------------'
  say 'Lvl  Bytes  Occurs     Usage      Name/Path '
  say '---- -----  --------  ---------- --------------------------------'

  /* Emit all top-level nodes */
  lineCnt = 0
  do ti = 1 to layouts.member.topCount
    node = layouts.member.top.ti
    call emitNodeSummary member, node, '', 0, showFiller, maxLines
    if maxLines>0 & lineCnt>=maxLines then leave
  end

  say '----------------------------------------------------------------'
  say 'Total lines printed:' lineCnt
  say '----------------------------------------------------------------'

return 0

/* Print all loaded members (in any order).
   Args:
     maxLines   - optional cap per member (0/blank = unlimited)
     showFiller - optional, 1=include FILLERs (default 0)
*/
printAllLayouts: procedure expose layouts. layout_loaded. loadedMembers.
  parse arg maxLines, showFiller
  if maxLines   = '' then maxLines   = 0
  if showFiller = '' then showFiller = 0
  count = 0
  do idx = 1 to loadedMembers.0
    memb = loadedMembers.idx
    if memb <> '' & layout_loaded.memb = 1 then do
      call printLayoutSummary memb, maxLines, showFiller
      count = count + 1
    end
  end
  if count = 0 then say 'printAllLayouts: no loaded members found.'
return 0

/* Recursive emitter for one node (no local named 'digits') */
emitNodeSummary: procedure expose layouts. lineCnt
  parse arg member, idx, path, depth, showFiller, maxLines
  lvl       = layouts.member.idx.level
  isGroup   = layouts.member.idx.isGroup
  name      = layouts.member.idx.name
  usage     = layouts.member.idx.usage
  pictype   = layouts.member.idx.pictype
  ndig      = layouts.member.idx.digit_s   /* <- schema tail is .digits */
  ndec      = layouts.member.idx.decs
  nbytes    = layouts.member.idx.bytes
  occMin    = layouts.member.idx.occMin
  occMax    = layouts.member.idx.occMax
  depOn     = layouts.member.idx.depOn
  /* Qualified path for display */
  fullName = name
  /*
  say 'DBG ndig=' ndig ' ndec=' ndec ' pictype=' pictype ' usage=' usage ' bytes=' nbytes
  say 'DBG path=' fullName
  */
  if path <> '' then fullName = path||'.'||name
  /* Optionally skip FILLER lines (but still traverse their children) */
  if translate(name) = 'FILLER' & showFiller = 0 then do
    if isGroup then do
      do ci=1 to layouts.member.idx.children.0
        ch = layouts.member.idx.children.ci
        call emitNodeSummary member, ch, path, depth+1, showFiller, maxLines
        if maxLines > 0 & lineCnt >= maxLines then return
      end
    end
    return
  end
  /* Compose columns */
  occStr = fmtOccurs(occMin, occMax, depOn)
  picStr = fmtPIC(pictype, ndig, ndec)
  if isGroup then do
    usageStr = 'GROUP'
    picShow  = ''
  end
  else do
    usageStr = usage
    picShow  = picStr
  end

  indent = copies(' ', depth*2)

  line = right(lvl,4) || ' ' right(nbytes,5) || ' ' || left(occStr,9) ||' ' || left(usageStr,10) || ' ' || left(indent||name, 33) || ' ' || picShow
  say line
  lineCnt = lineCnt + 1

  if maxLines > 0 & lineCnt >= maxLines then return

  /* Recurse into children for groups */
  if isGroup then do
    kids = layouts.member.idx.children.0
    do ci=1 to kids
      ch = layouts.member.idx.children.ci
      call emitNodeSummary member, ch, fullName, depth+1, showFiller, maxLines
      if maxLines>0 & lineCnt>=maxLines then return
    end
  end

return

/* Format OCCURS / DEPENDING ON */
fmtOccurs: procedure
  parse arg min, max, depOn
  if min=0 & max=0 & depOn='' then return ''
  if depOn<>'' then do
    if min=max then return min||' DEP:'||depOn
    else return min||'..'||max||' DEP:'||depOn
  end
  if min=max then return ''||min
  return min||'..'||max

/* Reconstruct PIC string from (pictype, ndig, ndec) */
fmtPIC: procedure
  parse arg pictype, ndig, ndec

  if pictype='ALPHA' then do
    if ndig = 0 then do
      return 'PIC X(?) '
    end
    return 'PIC X('||ndig||')'
  end
  if ndig = 0 then do
    if ndec > 0 then do
      return 'PIC 9(?)V9('||ndec||')'
    end
    else do
      return 'PIC 9(?)'
    end
  end
  pic = 'PIC 9('||ndig||')'
  if ndec > 0 then
    pic = pic||'V9('||ndec||')'
return pic
/* Display all -CT counters captured in ctVar. */
displayCtVar: procedure expose ctVar. ctVar_keys. ctVar_seen.
  say '--- ctVar (-CT counters) ---'
  shown = 0
  do i = 1 to ctVar_keys.0
    k = ctVar_keys.i
    v = ctVar.k
    if datatype(v,'N') then do
      say ' ' k ' = ' v
      shown = 1
    end
  end
  if shown = 0 then say ' (no -CT counters found)'
return 0
/* =============================================================== */
/* getCtForGroup: returns the numeric CT for a given group name.   */
/* Input : grpName  -> e.g. 'WS-CVCAT'                             */
/* Build : ctField  -> 'WS-CVCAT-CT'                               */
/*         keyName  -> 'WS_CVCAT_CT' (via safeTail)                */
/* Search: scans ctVar_keys. like displayCtVar to find keyName     */
/* Return: ctVar.keyName value if found; otherwise 0               */
/* =============================================================== */
getCtForGroup: procedure expose ctVar. ctVar_keys. ctVar_seen.
  parse arg grpName
  if grpName = '' then return 0
  grpU = strip(translate(grpName))

  if right(grpU, 3) = '-CT' then
    ctField = grpU
  else
    ctField = grpU || '-CT'       /* Step 2: e.g., WS-CVCAT-CT  */

  keyName = safeTail(ctField)     /* Step 3: WS_CVCAT_CT        */

  /* default when no match is found */
  notFoundDefault = 1
  tnum = notFoundDefault

  /* Make a safe numeric loop bound (portable on all TSO/E levels) */
  limit = 0
  if symbol('ctVar_keys.0') = 'VAR' then do
    if datatype(ctVar_keys.0,'N') then limit = ctVar_keys.0 + 0
  end

  /* Nothing captured yet -> return default (1) */
  if limit = 0 then return tnum

  /* Search like displayCtVar */
  do i = 1 to limit             /* Step 4: search like displayCtVar */
    if symbol('ctVar_keys.'i) <> 'VAR' then iterate
    k = ctVar_keys.i
    if k = keyName then do      /* Step 5: found -> return value   */
      v = ctVar.k
      if datatype(v,'N') then return v + 0
      /* Defensive: if value came as text, coerce digits */
      tdigits = onlyDigits(v)
      if tdigits = '' then tdigits = '0'
      return tdigits + 0
    end
  end
  /* No match -> default (1) */
  return tnum
/* ===============================================================
  getCtForFieldOrGroup: first tries a direct field counter from ctVar.,
  else falls back to the group's own <name>-CT via getCtForGroup(name).
   =============================================================== */
getCtForFieldOrGroup: procedure expose ctVar. ctVar_keys.
  parse arg depOnName, grpName
  key = safeTail(depOnName)
  if symbol('ctVar.'key) = 'VAR' then do
    v = ctVar.key
    if datatype(v,'N') then return v + 0
    t = onlyDigits(v)
    if t = '' then t = '0'
    return t + 0
  end
  /* Fallback: use the group's <name>-CT captured in ctVar. */
  return getCtForGroup(grpName)

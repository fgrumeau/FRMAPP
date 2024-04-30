**FREE
//%METADATA                                                                          *
// %TEXT Select a Familly code                                                       *
//%EMETADATA                                                                         *
Ctl-Opt nomain;

Dcl-F familly    Keyed;
Dcl-F famill1    Keyed
                 RENAME(ffami   :REC52);
Dcl-F fam301d    WORKSTN
                         INFDS(INFO)
                         SFILE(SFL01:RRN01)
                         indds(indds);
Dcl-Ds INFO;
  LRRN            BinDec(4:0)     Pos(378);
End-Ds;

/COPY ../QPROTOSRC/FAMILLY.RPGLEINC

Dcl-Ds indds;
  help            Ind             Pos(1);
  exit            Ind             Pos(3);
  prompt          Ind             Pos(4);
  refresh         Ind             Pos(5);
  create          Ind             Pos(6);
  cf08            Ind             Pos(8);
  cancel          Ind             Pos(12);
  morekeys        Ind             Pos(24);
  pagedown        Ind             Pos(25);
  sflclr          Ind             Pos(30);
  sfldsp          Ind             Pos(31);
  sfldspctl       Ind             Pos(32);
  sflnxtchg       Ind             Pos(33);
  dspatrri        Ind             Pos(34);
  InvalidOpt      Ind             Pos(35);
  OneSelect       Ind             Pos(36);
  bydesc          Ind             Pos(40);
  InvalidOptC     Ind             Pos(41);
  NotAvail        Ind             Pos(42);
  sflend          Ind             Pos(80);
End-Ds;

Dcl-S rrn01           Int(5);

Dcl-C prp             'prp';
Dcl-C lod             'lod';
Dcl-C dsp             'dsp';
Dcl-C key             'key';
Dcl-C chk             'chk';
Dcl-C chkctl          'chc';
Dcl-C actctl          'acc';
Dcl-C act             'act';


//=============================================
Dcl-Proc sltArtFam export;
  Dcl-Pi sltArtFam like(faid      );
    pcod            like(faid      );
  End-Pi;
  Dcl-S dft             like(faid      );
  Dcl-S step01          Char(3)         inz(prp);
  Dcl-S in08            Ind;
  Dcl-S sts01           Ind;
  Dcl-S rrs01           Int(5);
  Dcl-S teller          Int(5);
  Dcl-S err01           Ind;
  Dcl-S slt01           Ind;
  Dcl-S keycod          like(faid       );
  Dcl-S savcod          like(faid       );
  Dcl-S pdes            like(fadesc     );
  Dcl-S keydes          like(fadesc     );
  Dcl-S savdes          like(fadesc     );
if not %open(fam301d   );
  open fam301d    ;
endif;
keycod = pcod;
dft = pcod;
bydesc = *off;
dow step01 <> ' ';
  select ;
  when step01 = prp ;
    exsr s01prp;
  when step01 = lod ;
    exsr s01lod;
  when step01 = dsp ;
    exsr s01dsp;
  when step01 = key ;
    exsr s01key;
  when step01 = chk ;
    exsr s01chk;
  when step01 = act ;
    exsr s01act ;
  endsl;
enddo;
return dft;
//--- Clear Subfile  -----------------------------------------------------

begsr s01prp;
  RRS01 = 0;
  clear CTL01;
  sflclr = *on;
  sfldsp = *off;
  sfldspctl = *off;
  write CTL01;
  sflclr = *off;
  if not bydesc;
    setll KEYCOD familly   ;
    read familly;
    sflend = %EOF;
  else;
    setll KEYDES famill1   ;
    read famill1    ;
    SflEnd = %EOF;
  endif;
  SAVCOD = faid  ;
  SAVDES = fadesc;
  Step01 = lod;
endsr;
//--- Load Subfile  -----------------------------------------------------
begsr S01lod;
  RRN01 = RRS01;
  RRB01 = RRS01 + 1;
  TELLER = 0;
  OPT01 = 0;
  SflNxtChg = *OFF;
  faid   = SAVCOD;
  fadesc = SAVDES;
  dow not sflend
        and TELLER < 20;
    RRN01 = RRN01 + 1;
    TELLER = TELLER + 1;
    write SFL01;
    if not bydesc;
      read familly   ;
    else;
      read famill1   ;
    endif;
    SflEnd = %EOF;
  enddo;
  SAVCOD = faid  ;
  SAVDES = fadesc;
  RRS01 = RRN01;
  Step01 = dsp;
endsr;
//--- Display Subfile  --------------------------------------------------
begsr S01dsp;
  sfldsp = rrs01 > 0;
  sfldspctl = *on;
  write KEY01;
  exfmt CTL01;
  IN08 = CF08;
  RRB01 = LRRN;
  Step01 = key;
endsr;
//--- Command Keys  -----------------------------------------------------
begsr S01key;
  select;
  when exit;
    step01 = ' ';
  when cancel ;
    Step01 = ' ';
  when PageDown;
    Step01 = lod;
  other;
    Step01 = chk;
  endsl;
endsr;
//--- Check Subfile  ----------------------------------------------------
begsr S01chk;
  // SET FLAG SELECTION MADE OFF
  SLT01 = *OFF;
  // SET FLAG OPTION  MADE OFF
  STS01 = *OFF;
  // SET FLAG ERROR   MADE OFF
  ERR01 = *OFF;
  Step01 = act;
  readc(E) SFL01;
  SflNxtChg = *ON;
  dow not %error and not %eof;
    select;
    when OPT01 = 0;
      // NO ERROR
    when OPT01 <>1;
      STS01 = *ON;
      Step01 = dsp;
      dspatrRi = *on;
      InvalidOpt= *on;
      if ERR01 = *OFF;
        RRB01 = RRN01;
        ERR01 = *ON;
      endif;
      //OPTION = 1 BUT SELECTION ALREADY MADE
    when SLT01 = *ON;
      Step01 = dsp;
      dspatrRi = *on;
      OneSelect = *on;
      if ERR01 = *OFF;
        RRB01 = RRN01;
        ERR01 = *ON;
      endif;
    other;
      //OPTION = 1
      SLT01 = *ON;
      STS01 = *ON;
    endsl;
    update SFL01;
    dspatrRi = *off;
    readc SFL01;
  enddo;
  select;
  when OPTC1 <>8
        and OPTC1 <>0;
    InvalidOptC = *ON;
    Step01 = dsp;
  when SLT01 = *ON
        and OPTC1 <>0;
    NotAvail = *ON;
    Step01 = dsp;
  when STS01 = *ON
        and IN08 = *ON;
    Step01 = dsp;
  endsl;
endsr;
//--- Write Subfile  ----------------------------------------------------
begsr S01act;
  Step01 = dsp;
  select;
  when IN08 ;
    Step01 = prp;
    bydesc = not bydesc;
    if bydesc;
      clear KEYCOD;
    else;
      clear KEYDES;
    endif;
  when OPTC1 = 8;
    Step01 = prp;
    if not bydesc;
      KEYCOD = POSCOD;
    else;
      KEYDES = POSDES;
    endif;
  other;
    readc(E) SFL01;
    dow not %error and not %eof;
      if OPT01 = 1;
        return faid      ;
      endif;
      readc SFL01;
    enddo;
  endsl;
endsr;
                                                                                // 
End-Proc sltArtFam;
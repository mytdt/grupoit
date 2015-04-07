#INCLUDE "PCLR100.CH"
#INCLUDE "RWMAKE.CH"
#INCLUDE "PROTHEUS.CH"


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³PCLR100   º Autor ³ Welinton Fernandes º Data ³  13/06/01   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Relatorio que gera o LMC (Livro de Movimentacao de 		  º±±
±±º          ³ Combustivel)												  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ SIGALOJA                                                   º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function PCLR100()

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Private cString      := ""
Private aOrd         := {}
Private cDesc1       := STR0001 //"Este programa tem como objetivo imprimir relatorio "
Private cDesc2       := STR0002 //"de acordo com os parametros informados pelo usuario."
Private cDesc3       := STR0003 //"Livro de Movimentacao de Combustivel"
Private cPict        := ""
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private limite       := 80
Private tamanho      := "P"
Private nomeprog     := "PCLR100" // Coloque aqui o nome do programa para impressao no cabecalho
Private nTipo        := 18
Private aReturn      := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey     := 0
Private cPerg        := "PCLR100"
Private titulo       := STR0003 //"Livro de Movimentacao de Combustivel"
Private nLin         := 80
Private Cabec1       := ""
Private Cabec2       := ""
Private cbtxt        := Space(10)
Private cbcont       := 00
Private CONTFL       := 01
Private m_pag        := 01
Private imprime      := .T.
Private wnrel        := "PCLR100" // Coloque aqui o nome do arquivo usado para impressao em disco

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis contadoras			                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Private cFOLHA  := Space(3)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Validacao a Autorizacao de Uso de Template                          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
// cGrupo, cOrdem, cPergunt, cPerSpa, cPerEng, cVar, cTipo ,nTamanho, nDecimal, nPresel, cGSC, cValid, cF3, cGrpSxg, cPyme, cVar01, cDef01, cDefSpa1, cDefEng1, cCnt01, cDef02, cDefSpa2, cDefEng2, cDef03, cDefSpa3, cDefEng3, cDef04, cDefSpa4, cDefEng4, cDef05, cDefSpa5, cDefEng5, aHelpPor, aHelpEng, aHelpSpa, cHelp
PutSx1(cPerg, "01", "Data do Livro de ", "", "", "mv_ch1", "D", 08, 00, 00, "G", "", "   ", "", "", "MV_PAR01", "    ", "", "", "", "    ", "", "", "", "", "", "", "", "", "", "", "", {}, {}, {}, "")
PutSx1(cPerg, "02", "Data do Livro ate", "", "", "mv_ch2", "D", 08, 00, 00, "G", "", "   ", "", "", "MV_PAR02", "    ", "", "", "", "    ", "", "", "", "", "", "", "", "", "", "", "", {}, {}, {}, "")
PutSx1(cPerg, "03", "Do Produto       ", "", "", "mv_ch3", "C", 15, 00, 00, "G", "", "SB1", "", "", "MV_PAR03", "    ", "", "", "", "    ", "", "", "", "", "", "", "", "", "", "", "", {}, {}, {}, "")
PutSx1(cPerg, "04", "Folha            ", "", "", "mv_ch4", "C", 05, 00, 00, "G", "", "   ", "", "", "MV_PAR04", "    ", "", "", "", "    ", "", "", "", "", "", "", "", "", "", "", "", {}, {}, {}, "")

If !Pergunte(cPerg,.T.)
	Return()
EndIf

wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.T.,Tamanho,,.F.)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta a interface padrao com o usuario...                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString,,,Tamanho,1)

If nLastKey == 27
	Return
Endif

nTipo := If(aReturn[4]==1,15,18)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Processamento. RPTSTATUS monta janela com a regua de processamento. ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFun??o    ³RUNREPORT º Autor ³ Welinton           º Data ³  13/06/01   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescri??o ³ Funcao auxiliar chamada pela RPTSTATUS. A funcao RPTSTATUS º±±
±±º          ³ monta a janela com a regua de processamento.               º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Programa principal                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)

Local aColsEst, aColsVen, aSaldo, aTanques, aPergs, aRet, aNotas
Local nI, nC, nPos, nColuna, nCol
Local nSldAber, nSldFecha, nQtdTotal, nSaldoIni, nSaldoFim, nVolDisp, nVlrTotal
Local nQuant, nVlrItem, nQtdDia, nQtdAfer, nVlrDia, nVolVend, nQtdFinal, nPerGan, nAferic
Local cFiltro, cFilSL1, cFilSL2, cTqObs, cObs, cEncIni, cEncFin, cLocal, cBico, cDesc
Local dDataAux
Local lAppend

SetRegua(0)

dbSelectArea("SM0")
dbSeek(cEmpAnt+cFilAnt)
Cabec1 := Alltrim(SUBSTR(SM0->M0_NOMECOM,1,35)) + " - " + Alltrim(SM0->M0_FILIAL)+ " / CNPJ "+Transform(SM0->M0_CGC,"@R 99.999.999/9999-99")
	
dbSelectArea("SD1")
dbSetOrder(7) //D1_FILIAL+D1_COD+D1_LOCAL+DTOS(D1_DTDIGIT)+D1_NUMSEQ

dbSelectArea("SB1")
dbSetOrder(1) //B1_FILIAL+B1_COD
SB1->(dbSeek(xFilial("SB1")+MV_PAR03))

dbSelectArea("LEI")
cFiltro := "LEI_FILIAL == '"+xFilial("LEI")+"'"
cFiltro	+= ".And.LEI_PROD == '"+MV_PAR03+"'"	
LEI->(DbSetFilter({||&cFiltro}, cFiltro))
LEI->(dbGoTop())

aTanques := {}
Do While LEI->(!EOF())	
	nPos := aScan(aTanques,{|x|x[2]==LEI->LEI_TANQUE})
	If nPos == 0
		aAdd(aTanques,{LEI->LEI_PROD,LEI->LEI_TANQUE,PadR(LEI->LEI_BICO,2),999999})
	Else
		If !(LEI->LEI_BICO $ aTanques[nPos][3])
			aTanques[nPos][3] += ',' + PadR(LEI->LEI_BICO,2)
		EndIf
	EndIf
	LEI->(dbSkip())
EndDo
	
SetRegua(DateDiffDay(MV_PAR01,MV_PAR02)+1)

nVlrTotal := 0
While MV_PAR01 <= MV_PAR02
	
	aPergs	:= {}
	aRet	:= {}
	cObs	:= Space(TamSX3("LMC_OBS")[1])
	dDataAux:= MV_PAR01
	
	dbSelectArea("LMC")
	dbSetOrder(1)
	
	lAppend := !LMC->(dbSeek(xFilial("LMC")+SB1->B1_COD+Dtos(MV_PAR01)))
	
	If !lAppend
		cObs := LMC->LMC_OBS
	EndIf
	
	aAdd( aPergs ,{1,"Observação (" + DTOC(dDataAux) + ") : ",cObs,"@!",'.T.',,'.T.',80,.F.})
	
	If ParamBox(aPergs ,"Parametros ",aRet)
		
		cObs := aRet[1]
		RecLock("LMC",lAppend)
		
		If lAppend
			LMC->LMC_FILIAL := xFilial("LMC")
			LMC->LMC_PROD 	:= SB1->B1_COD
			LMC->LMC_DATA	:= dDataAux
		EndIf
		
		LMC->LMC_OBS	:= cObs
		MsUnLock()
		
	EndIf
	
	nSldAber	:= 0
	nSldFecha	:= 0
	nQtdTotal	:= 0
	nCol		:= 0
	nColuna		:= 0
	aSaldo		:= {}
	aColsEst 	:= {}
	
	MV_PAR01 := dDataAux
	
	For nC := 1 to Len(aTanques)
		
		aSaldo 		:= CalcEst(aTanques[nC][1],aTanques[nC][2],MV_PAR01)
		nSaldoIni 	:= aSaldo[1]
		nSldAber	+= nSaldoIni
		
		aSaldo 	 	:= CalcEst(aTanques[nC][1],aTanques[nC][2],MV_PAR01+1)
		nSaldoFim	:= aSaldo[1]
		
		nSldFecha	+= nSaldoFim
		
		Aadd(aColsEst,{"|TQ-"+aTanques[nC][2],nSaldoIni,nCol,nSaldoFim,aTanques[nC][1],aTanques[nC][2]} )
		nCol += 11
		
	Next nC
	
	cFOLHA := Val(MV_PAR04)
	
	If nLin > 55
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 8
	EndIf
	
	@ Prow()+1,001 Psay "+-----------------------------------------------------------------------------+"
	
	@ Prow()+1,001 Psay STR0008 + STR0009 + DTOC(GravaData(MV_PAR01,.T.,1)) +"  |"
	
	cDESC := Alltrim(SB1->B1_DESC)
	
	@ Prow()+1,001 Psay STR0010 + Alltrim(MV_PAR03) + " - " + cDESC
//	@ Prow(),64    Psay STR0011 + Strzero(cFOLHA,5,0) + "|" //"| [1] Produto: "###" Folha No:"
	@ Prow(),58    Psay "Folha No : " + Strzero(cFOLHA,5,0) + Space(5) + "|" //"| [1] Produto: "###" Folha No:"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay "|       [3] Estoque de Abertura (Medicao Fisica no Inicio do Dia)             |"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	// 01234567890123456789012345678901234567890123456789012345678901234567890123456789
	// |TQ-01     |TQ-01     |TQ-01     |TQ-01     |TQ-01     |[3.1]Estoque Abertura |
	// |999999.999|999999.999|999999.999|999999.999|999999.999|      999,999,999.999 |
	@ Prow()+1,000 Psay ""
	
	For nI := 1 To Len(aColsEst)
		nColuna := (aColsEst[nI][3]+1)
		@ Prow(), nColuna Psay aColsEst[nI][1]
	Next
	
	@ Prow(), nColuna+12  Psay "|"
	@ Prow(),57 Psay "|[3.1]Estoque Abertura|"
	@ Prow()+1,000 Psay ""
	
	For nI := 1 To Len(aColsEst)
		nColuna := (aColsEst[nI][3]+1)
		@ Prow(), nColuna Psay "|"
		@ Prow(), nColuna+1 Psay aColsEst[nI][2] Picture "@E 999999.999"
	Next
	
	@ Prow(), nColuna+12  Psay "|"
	@ Prow(),057 Psay "|"
	@ Prow(),060 Psay nSldAber Picture "@E 999,999,999,999.999" + "|"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0014 //"|[4] Volume Recebido no Dia (Em Litros) |[4.1] No TQ Desc.|[4.2] Volume Rece. |"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	aNotas  := {}
	For nC := 1 To Len(aColsEst)      
		SD1->(dbGoTop())
		If SD1->(MsSeek(xFilial("SD1")+aColsEst[nC][5]+aColsEst[nC][6]+DTOS(MV_PAR01)))
			While SD1->(!EOF() .And. D1_FILIAL+D1_COD+D1_LOCAL+DTOS(D1_DTDIGIT) == xFilial("SD1")+aColsEst[nC][5]+aColsEst[nC][6]+DTOS(MV_PAR01))
				
				If SD1->D1_TIPO != 'D'
					nPos := aScan(aNotas, {|x| x[1] == SD1->D1_DOC .and. x[2] == SD1->D1_LOCAL })
					If nPos > 0
						aNotas[nPos][3] += SD1->D1_QUANT
					Else
						aAdd(aNotas, {SD1->D1_DOC,SD1->D1_LOCAL,SD1->D1_QUANT})
					EndIf
					
					nQtdTotal += SD1->D1_QUANT
				EndIf
				
				SD1->(dbSkip())
			EndDo
		EndIf
	Next nC
	
	If nQtdTotal = 0
		//@ Prow()+1,001 Psay STR0015 + Space(10) + " de " + Space(08) + "| " + Space(16) + "|" + Space(19) + "|" //"| Nota Fiscal Nr. "
		@ Prow()+1,001 Psay "|" + Space(Len(STR0015)-1) + Space(10) + Space(4) + Space(08) + "| " + Space(16) + "|" + Space(19) + "|" //"| Nota Fiscal Nr. "
	Else
		For nI := 1 to Len(aNotas)
			@ Prow()+1,001 Psay STR0015 + aNotas[nI][1]  + " de " + DTOC(GravaData(MV_PAR01,.T.,1)) + " | " + aNotas[nI][2]
			@ Prow(),059 Psay "|"
			@ Prow(),060 Psay aNotas[nI][3] Picture "@E 999,999,999,999.999" + "|"
		Next nI
	Endif
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0016 //"|                                    [4.3] Total Recebido |"
	@ Prow(),060 Psay nQtdTotal Picture "@E 999,999,999,999.999" + "|"
	
	nVolDisp	:= nSldAber+nQtdTotal
	
	@ Prow()+1,001 Psay STR0017 //"|                   [4.4] Volume Disponivel (3.1) + (4.3) |"
	@ Prow(),060 Psay nVolDisp Picture "@E 999,999,999,999.999" + "|"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0018 //"|              [5] Volume Vendido no Dia ( Em Litros )                        |"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0019 //"|[5.1] TQ|[5.2] Bico|[5.3] + Fech.|[5.4] - Abert.|[5.5]-Afr.|[5.6]=Vendas Bico|"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
		
	dbSelectArea("SL1")
	dbSetOrder(1) //L1_FILIAL+L1_NUM
	
	dbSelectArea("SL2")
	dbSetOrder(1) //L2_FILIAL+L2_NUM+L2_ITEM+L2_PRODUTO
	
	cFilSL1 := "L1_FILIAL == '"+xFilial("SL1")+"' "
	cFilSL1	+= ".And.Dtos(L1_EMISNF) = '"+Dtos(mv_par01)+"' "
	
	SL1->(DbSetFilter({||&cFilSL1}, cFilSL1))
	SL1->(dbGoTop())
	
	cFilSL2 := "L2_FILIAL == '"+xFilial("SL2")+"' "
	cFilSL2	+= ".And.Dtos(L2_EMISSAO) = '"+Dtos(mv_par01)+"' "
	
	SL2->(DbSetFilter({||&cFilSL2}, cFilSL2))
	SL2->(dbGoTop())
	
	aColsVen := {}
	
	While SL1->(!EOF())
		
		SL2->(dbGoTop())
		SL2->(dbSeek(xFilial("SL2")+SL1->L1_NUM))
		
		While SL2->(!EOF() .And. L2_FILIAL+L2_NUM == xFilial("SL2")+SL1->L1_NUM)
			
			If !Empty(SL2->L2_TBICO) .And. SL2->L2_VENDIDO == "S" .And. Alltrim(SL2->L2_PRODUTO)  == Alltrim(MV_PAR03)
				
				nPos 	:= Ascan(aColsVen,{|x| x[1]+x[2] == SL2->L2_LOCAL + SL2->L2_TBICO})
				
				nEncIni := SL2->L2_TENCINI
				nEncFin := SL2->L2_TENCFIM
				cLocal	:= SL2->L2_LOCAL
				cBico	:= SL2->L2_TBICO
				
				If SL1->L1_TAFERIC == "S"
					nAferic := SL2->L2_QUANT
					nQuant  := 0
					nVlrItem:= 0
				Else
					nAferic := 0
					nQuant  := SL2->L2_QUANT
					nVlrItem:= SL2->L2_VLRITEM
				Endif
				
				If nPos > 0
					
					aColsVen[nPos,3] +=  nAferic
					aColsVen[nPos,4] +=  nQuant
					aColsVen[nPos,5] +=  nVlrItem
					
					If aColsVen[nPos][6] > SL2->L2_TENCINI
						aColsVen[nPos][6] := SL2->L2_TENCINI
					EndIf
					
					If aColsVen[nPos][7] < SL2->L2_TENCFIM
						aColsVen[nPos][7] := SL2->L2_TENCFIM
					EndIf
					
				Else
					Aadd(aColsVen,{cLocal,cBico,nAferic,nQuant,nVlrItem,nEncIni,nEncFin})
				Endif
				
			EndIf
			SL2->(dbSkip())
		EndDo
		
		SL1->(dbSkip())
	EndDo
	
	SL1->(dbClearFilter())
	SL2->(dbClearFilter())
	
	aSort(aColsVen,,,{|x,y| x[1]+x[2] < y[1]+y[2]})
	
	nQtdTotal := 0
	//nVlrTotal := 0
	nQtdDia  := 0
	nQtdAfer := 0
	nVlrDia  := 0
	
	//          1         2         3         4         5         6         7
	// 1234567890123456789012345678901234567890123456789012345678901234567890123456789
	// |                                           |    + Ganhos (*)
	// |Vlr.Acumulado no Mes                       |[7]Est.Fechamento"
	// |Vlr.Ven.Dia(5.7 X Prc.Bomba)               |[6]Est.Escrit.(4.4-5.7)
	// |[10] Valor das Vendas (R$)                 |[5.7] Vendas no Dia
	// |[5.1] TQ|[5.2] Bico|[5.3] + Fech.|[5.4] - Abert.|[5.5]-Afr.|[5.6]=Vendas Bico|
	
	For n:= 1 To Len(aColsVen)
		
		@ Prow()+1,001 Psay "| " + aColsVen[n][1] + "     | " + aColsVen[n][2] + "       |"
		@ Prow(),022 Psay aColsVen[n][7] Picture "@E 9,999,999.999" + "|" //Fechamento
		@ Prow(),037 Psay aColsVen[n][6] Picture "@E 9,999,999.999" + "|" //Abertura
		@ Prow(),051 Psay aColsVen[n][3] Picture "@E 99,999.999" + "|"
		
		nVolVend := aColsVen[n][7]-aColsVen[n][6]-aColsVen[n][3]
		
		@ Prow(),062 Psay nVolVend Picture "@E 9,999,999,999.999" + "|"
		
		nQtdDia  += aColsVen[n][4]
		nVlrDia  += aColsVen[n][5] 
		nQtdAfer += aColsVen[n][3]
				
	Next n
	
	nVlrTotal += nVlrDia
	nQtdDia := nQtdDia - nQtdAfer
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	@ Prow()+1,001 Psay STR0020 //"|[10] Valor das Vendas (R$)                 |[5.7] Vendas no Dia"
	@ Prow(),066 Psay nQtdDia	Picture "@E 9,999,999.999" + "|"
	
	
	@ Prow()+1,001 Psay STR0021 //"|Vlr.Ven.Dia(5.7 X Prc.Bomba)"
	@ Prow(),031 Psay nVlrDia Picture "@E 99,999,999.999" + "|"
	
	@ Prow(),046 Psay STR0022 //"[6]Est.Escrit.(4.4-5.7)"
	@ Prow(),070 Psay nVolDisp-nQtdDia  Picture "@E 99999.999" + "|"
	
	nPerGan := (nSldFecha - (nVolDisp-nQtdDia) )  // Variavel que armazena: Valores de Perdas e Ganhos
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0023 //"|Vlr.Acumulado no Mes         "
	@ Prow(),031 Psay nVlrTotal Picture "@E 99,999,999.999" + "|"
	
	@ Prow(),046 Psay STR0024 //"[7]Est.Fechamento"
	@ Prow(),064 Psay nSldFecha  Picture "@E 999,999,999.999" + "|"
	
	@ Prow()+1,001 Psay "|-------------------------------------------|---------------------------------|"
	
	@ Prow()+1,001 Psay STR0025 //"|[11] Para Uso do Revendedor                |[8] - Perdas                     |"
	@ Prow()+1,001 Psay STR0026 //"|                                           |    + Ganhos (*)"
	@ Prow(),064 Psay nPerGan Picture "@E 999,999,999.999" + "|"
	
	@ Prow()+1,001 Psay "|-------------------------------------------|---------------------------------|"
	
	@ Prow()+1,001 Psay STR0027 //"|[13] Observacoes                           |[12]Destinado a Fiscalizacao     |"
	
	cTqObs := ''
	For nI := 1 to Len(aTanques)
		//43 caracteres é uma linha
		cTqObs += 'Tanque: ' + PadR(aTanques[nI][2],2) + ' - Capacidade: ' + Transform(aTanques[nI][4], '@E 999,999') + Space(11)
		nC := 1
		While nC < Len(aTanques[nI][3])
			cTqObs += '> Bico(s): ' + PadR(SubStr(aTanques[nI][3], nC, 32), 32)
			nC += 32
		EndDo
	Next nI
	
	cObs := PADR(cTqObs+cObs,344)
		
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,001,43) + "|---------------------------------|"
	
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,044,43) + "|ANP "+ Space(29) + "|"
	
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,087,43) + "|" + Space(33)+ "|"
	
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,130,43) + "|" + Space(33)+ "|"
	
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,173,43) + "|---------------------------------|"
	
	@ Prow()+1,001 Psay "|"	+ SubsTr(cObs,216,43) + STR0028 //"|OUTROS ORGAOS FISCAIS            |"
	
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,259,43) + "|" + Space(33)+ "|"
	
	@ Prow()+1,001 Psay "|" + SubsTr(cObs,302,43) + "|" + Space(33)+ "|"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0029 //"|                          Conciliacao dos Estoques                           |"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,000 Psay ""
	
	For nI := 1 To Len(aColsEst)
		nColuna := aColsEst[nI][3]
		@ Prow(), nColuna+1 Psay aColsEst[nI][1]
	Next
	@ Prow(), nColuna+12  Psay "|"
	
	@ Prow(),57 Psay "|                     |"
	@ Prow()+1,000 Psay ""
	
	nQtdFinal	:= 0
	
	For nI := 1 To Len(aColsEst)
		nColuna := (aColsEst[nI][3]+1)
		@ Prow(), nColuna   Psay "|"
		@ Prow(), nColuna+1 Psay aColsEst[nI][4] Picture "@E 999999.999"
		nQtdFinal	+= aColsEst[nI][4]
	Next
	
	@ Prow(), nColuna+11  Psay "|"
	@ Prow(),57 Psay "|     T O T A L       |"
	
	// 01234567890123456789012345678901234567890123456789012345678901234567890123456789
	// |TQ-01     |TQ-01     |TQ-01     |TQ-01     |TQ-01     |     T O T A L        |
	// |999999.999|999999.999|999999.999|999999.999|999999.999|[9.1] 999,999,999.999	|
	
	@ Prow()+1,001 Psay "|-------------------------------------------------------|                     |"
	@ Prow()+1,001 Psay "|[9] Fechamento|----------------------------------------|[9.1]"
	
	@ Prow(),064 Psay nSldFecha Picture "@E 999,999,999.999" + "|"
	
	@ Prow()+1,001 Psay STR0031 //"|     Fisico   |--------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay "|-----------------------------------------------------------------------------|"
	
	@ Prow()+1,001 Psay STR0032 //"|(*ATENCAO)-Se o resultado for negativo, pode estar havendo vazamento de      |"
	
	@ Prow()+1,001 Psay STR0033 //"|                       produto para o meio ambiente                          |"
	
	@ Prow()+1,001 Psay "+-----------------------------------------------------------------------------+"
	
	IncRegua()
	
	MV_PAR01 += 1
	MV_PAR04 := Soma1(MV_PAR04)
	nLin := 80
EndDo

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Finaliza a execucao do relatorio...                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

SET DEVICE TO SCREEN

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Se impressao em disco, chama o gerenciador de impressao...          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

If aReturn[5]==1
	dbCommitAll()
	SET PRINTER TO
	OurSpool(wnrel)
Endif

MS_FLUSH()

Return
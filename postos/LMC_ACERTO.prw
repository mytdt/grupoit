#include "rwmake.ch"
#include "protheus.ch"
#include "topconn.ch"
#include "colors.ch"

/*---------------------------------------------------------------------------
| Programa : LMC_ACERTO.PRW   | Autor : IONAI M. CARMO    | Data : 10/03/15 |
-----------------------------------------------------------------------------
| Descrição: Ajuste na Movimentação para LCM                                |
-----------------------------------------------------------------------------
| Alteração: Utilizado no TEMPLATE POSTO / SIGALOJA                         |
----------------------------------------------------------------------------- 
*/                                                                         

User Function LMC_ACERTO()

_cPerg  := "LMCACERTO "

CriaPerg()  // Criacao de perguntas SX1

lPerg := Pergunte(_cPerg,.T.)

If lPerg
   Processa({|| ProcQuery()},"Selecionando registros...")
EndIf

Return(nil)

//**************************************************************************************************************************************************
Static Function ProcQuery()

dDataDe := mv_par01
cLocDe  := mv_par02
cLocAte	:= mv_par03
cPrdDe  := mv_par04
cPrdAte	:= mv_par05

If Select("SQL") > 0
   SQL->(dbCloseArea())
EndIf 

/*
cQry := "SELECT L2_EMISSAO EMISSAO,L2_LOCAL TANQUE,L2_DESCRI PRODUTO,SUM(L2_QUANT) QUANT,SUM(L2_TOTIMP) QTD_LMC,SUM(L2_VLIMPOR) DIFERENCA,L2_PRODUTO CODIGO " + chr(10)
cQry += "FROM "+RetSqlName("SL2")+" WHERE " + chr(10)
cQry += "L2_FILIAL  = '"+cFilAnt+"' AND " + chr(10) 
cQry += "L2_EMISSAO = '"+dtos(dDataDe)+"' AND " + chr(10)
cQry += "L2_LOCAL	BETWEEN '"+cLocDe +"' AND '"+cLocAte +"' AND " + chr(10)
cQry += "L2_PRODUTO	BETWEEN '"+cPrdDe +"' AND '"+cPrdAte +"' AND " + chr(10)
cQry += "LEN(L2_TBICO) > 0 AND D_E_L_E_T_ <> '*' " + chr(10) 
cQry += "GROUP BY L2_EMISSAO,L2_DESCRI,L2_LOCAL,L2_PRODUTO " + chr(10)  
cQry += "ORDER BY L2_EMISSAO,L2_DESCRI,L2_LOCAL"  
*/

cQry := CRLF + " SELECT"
cQry += CRLF + "    L2_EMISSAO EMISSAO"
cQry += CRLF + "   ,L2_LOCAL TANQUE"
cQry += CRLF + "   ,B1_DESC PRODUTO"
cQry += CRLF + "   ,L2_PRODUTO CODIGO"
cQry += CRLF + " FROM " + RetSqlName("SL2") + " SL2"
cQry += CRLF + " LEFT JOIN " + RetSqlName("SB1") + " SB1"
cQry += CRLF + " ON  SB1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND B1_FILIAL = '" + xFilial('SB1') + "'"
cQry += CRLF + " AND B1_COD = L2_PRODUTO"
cQry += CRLF + " WHERE SL2.D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND L2_FILIAL = '" + xFilial('SL2') + "'"
cQry += CRLF + "   AND L2_EMISSAO = '" + DTOS(dDataDe) + "'"
cQry += CRLF + "   AND L2_LOCAL BETWEEN '" + cLocDe + "' AND '" + cLocAte + "'"
cQry += CRLF + "   AND L2_PRODUTO BETWEEN '" + cPrdDe + "' AND '" + cPrdAte + "'"
cQry += CRLF + "   AND LEN(L2_TBICO) > 0"
cQry += CRLF + " GROUP BY"
cQry += CRLF + "    L2_EMISSAO"
cQry += CRLF + "   ,B1_DESC"
cQry += CRLF + "   ,L2_LOCAL"
cQry += CRLF + "   ,L2_PRODUTO"
cQry += CRLF + " ORDER BY"
cQry += CRLF + "    L2_EMISSAO"
cQry += CRLF + "   ,B1_DESC"
cQry += CRLF + "   ,L2_LOCAL"
cQry += CRLF + "   ,L2_PRODUTO"

TCQuery cQry ALIAS "SQL" NEW 

TcSetField("SQL","EMISSAO","D",08,00)
TcSetField("SQL","TANQUE" ,"C",02,00)
TcSetField("SQL","PRODUTO","C",45,00)
TcSetField("SQL","CODIGO" ,"C",15,00)

cArqTMP := CriaTrab(NIL,.F.)
Copy To &cArqTMP

dbCloseArea()
dbUseArea(.T.,,cArqTMP,"SQL",.T.)                    

dbSelectArea("SQL")

If RecCount() > 0
   Processa({|| ProcBrowse()},"Selecionando registros...")
EndIf

dbSelectArea("SQL")
dbCloseArea()

cArqTemp := cArqTMP + '.DBF'
Delete File &cArqTemp

Return(nil)

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Static Function ProcBrowse()

vSvAlias:={Alias(),IndexOrd(),Recno()}

dEmissao:= dDataDe

dbSelectArea("SX3")
dbSetOrder(1)

nUsado:=0
aHeader:={}
aFields:= {}
nOpcx  := 3
ALTERA := .T.
INCLUI := .T.

#define MY_TANQUE      01
#define MY_PRODUTO     02
#define MY_SALDO_FINAL 03
#define MY_VEEDER_ROOT 04
#define MY_DIFERENCA   05
#define MY_DESCRICAO   06
#define MY_FINAL       07

AADD(aFields,{"Tanque"     ,"L2_LOCAL"  ,"L2_LOCAL"   })
AADD(aFields,{"Produto"    ,"L2_PRODUTO","L2_PRODUTO" })
AADD(aFields,{"Saldo Final","L2_QUANT"  ,"nSaldoFinal"})
AADD(aFields,{"Veeder Root","L2_QUANT"  ,"nVeederRoot"})
AADD(aFields,{"Diferença"  ,"L2_QUANT"  ,"nDiferenca" })
AADD(aFields,{"Descrição"  ,"B1_DESC"   ,"B1_DESC"    })

For i:=1 To Len(aFields)
	dbSelectArea("SX3")
	dbSetOrder(2)
	If dbSeek(aFields[i][2])           
       lValida := ".T."
   	   nUsado++
	   AADD(aHeader,{ aFields[i][1], aFields[i][3], x3_picture, x3_tamanho, x3_decimal, lValida,;
					  x3_usado, x3_tipo, x3_arquivo, x3_context } )
	EndIf
Next i

// Montando aCols                                               
aCols :={}
nCnt  := 1
nTotal:= 0

dbSelectArea("SQL")
dbGoTop()

While !Eof()
	   AADD(aCols,Array(MY_FINAL))
	   i := len(aCols)
                                                   
	   aSdFim  := CalcEst(SQL->CODIGO,SQL->TANQUE,dDataDe+1)
	   nSldFim := aSdFim[1]
	   //aSdIni  :=  CalcEst(SQL->CODIGO,SQL->TANQUE,dDataDe)
	   //nSldIni := aSdIni[1]
	   dbSelectArea("SQL")                             
	   aCols[i][MY_TANQUE]      := SQL->TANQUE
	   aCols[i][MY_PRODUTO]     := SQL->CODIGO
	   aCols[i][MY_SALDO_FINAL] := nSldFim
	   aCols[i][MY_VEEDER_ROOT] := 0
	   aCols[i][MY_DIFERENCA]   := 0
	   aCols[i][MY_DESCRICAO]   := SQL->PRODUTO
	   aCols[i][MY_FINAL]    := .f.
	   
	   nTotal += nSldFim
	   nCnt := nCnt + 1   
	   
	   dbSkip()
End
	
cTitulo:="Movimentação Posto - LMC"
lCab   := .f.
lRod   := .t.
	
aTela  :={150,010,400,620}
	
aC:={}
AADD(aC,{"dEmissao",{15,003}," Emissão",,,,lCab})
AADD(aC,{"nTotal"  ,{15,075}," Total","@E 99,999,999,999,999.999",,,lCab})
aR     :={}
aCGD   :={30,05,120,300}
aGetCpo:= {"nVeederRoot"}

cLinhaOk := "U_MyDifLMC()"
cTudoOk  := ".T."
	
lRetMod2:=Modelo2(cTitulo,aC,aR,aCGD,nOpcx,cLinhaOk,cTudoOk,aGetCpo,,,,aTela,.T.)
	
If lRetMod2
    cDoc := GetSx8Num("SD3")
    lReg := .f.
	For i:=1 to Len(aCols)
		If !aCols[i][nUsado+1]
		    If aCols[i][MY_DIFERENCA] <> 0
				cNumseq := ProxNum()    
				nQuant  := aCols[i][MY_DIFERENCA] * If(aCols[i][MY_DIFERENCA]<0,-1,1)
				dbSelectArea("SB1")
				dbSeek(xFilial("SB1")+aCols[i][MY_PRODUTO])
				dbSelectArea("SD3")
				RecLock("SD3",.T.)
				Replace	D3_FILIAL  With cFilAnt,;
						D3_COD     With aCols[i][MY_PRODUTO],;
						D3_DOC     With cDoc,;
						D3_EMISSAO With dDataDe,;
						D3_GRUPO   With SB1->B1_GRUPO,;
						D3_LOCAL   With aCols[i][MY_TANQUE],;
						D3_UM      With SB1->B1_UM,;
						D3_NUMSEQ  With cNumSeq,;
						D3_SEGUM   With SB1->B1_SEGUM,;
						D3_CONTA   With SB1->B1_CONTA,;
						D3_QUANT   With nQuant,;
						D3_TIPO    With SB1->B1_TIPO,;
						D3_LOCALIZ With SB2->B2_LOCALIZ,;
						D3_USUARIO With SubStr(cUsuario,7,15),;
						D3_DTVALID With dDataDe      
						If aCols[i][MY_DIFERENCA] < 0
							Replace D3_TM With "999"
							Replace D3_CF With "RE0"        
							cCH := "E9"
						Else
							Replace D3_TM With "499"
							Replace D3_CF With "DE0"
							cCH := "E0"
						EndIf
						Replace D3_CHAVE With cCH     
						Replace D3_OBS1  With "AJT LMC"
			    MsUnLock()
			    lReg := .t.
		        
				dbSelectArea("SB2")     
				dbSetOrder(1)
				If dbSeek(xFilial("SB2")+SD3->D3_COD)    
				    nSld := B2_QATU         
				    If SD3->D3_CF = "RE0"
				       nSld -= SD3->D3_QUANT
				    Else
				       nSld += SD3->D3_QUANT
				    EndIf   
					RecLock("SB2",.F.)
					Replace B2_QATU    With nSld
					Replace B2_VATU1   With 0
					MsUnlock()
				EndIf	
			EndIf	
		Endif
	Next I
	If lReg
       ConfirmSX8()    
    Else
	   RollBackSx8()
    EndIf
Endif

dbSelectArea(vSvAlias[1])
dbSetOrder(vSvAlias[2])
dbGoto(vSvAlias[3])

Return

// *****************************************************************************************************************************************************************************
Static Function CriaPerg()
           
aSvAlias:={Alias(),IndexOrd(),Recno()}
i:=j:=0

aRegistros:={}

AADD(aRegistros,{_cPerg,"01","Data 		 ","","","mv_ch1","D",08,00,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
AADD(aRegistros,{_cPerg,"02","Do  Tanque ","","","mv_ch2","C",02,00,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","Z1","","","","",""})
AADD(aRegistros,{_cPerg,"03","Até Tanque ","","","mv_ch3","C",02,00,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","Z1","","","","",""})
AADD(aRegistros,{_cPerg,"04","Do  Produto","","","mv_ch4","C",20,00,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","SB1","","","","",""})
AADD(aRegistros,{_cPerg,"05","Até Produto","","","mv_ch5","C",20,00,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","SB1","","","","",""})

dbSelectArea("SX1")
For i := 1 to Len(aRegistros)
    dbSeek(aRegistros[i,1]+aRegistros[i,2])
    If !Found()
       While !RecLock("SX1",.T.)
       End
       For j:=1 to FCount()
	       FieldPut(j,aRegistros[i,j]) 
       Next
       MsUnlock()
    Endif        
Next i

dbSelectArea(aSvAlias[1])
dbSetOrder(aSvAlias[2])
dbGoto(aSvAlias[3])

Return(nil)

/* ------------ */

User Function MyDifLMC()

Local lRet := .T.
Local oGetD := CallMod2Obj()

If aCols[oGetD:oBrowse:nAt][MY_VEEDER_ROOT] > 0
	aCols[oGetD:oBrowse:nAt][MY_DIFERENCA] := aCols[oGetD:oBrowse:nAt][MY_VEEDER_ROOT] - aCols[oGetD:oBrowse:nAt][MY_SALDO_FINAL]
Else
	aCols[oGetD:oBrowse:nAt][MY_DIFERENCA] := 0
EndIf

oGetD:oBrowse:Refresh()

Return lRet
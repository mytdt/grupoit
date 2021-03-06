#include "rwmake.ch"
#include "protheus.ch"

///////////////////////////////////////////////////////////////////////////////
User Function MyDIEF2P()
///////////////////////////////////////////////////////////////////////////////
// Data : 02/04/2015
// User : Thieres Tembra
// Desc : Gera relat�rio da DIEF/PA Anexo II referente a 
//        Op. de Postos Rev. de Combust�vel
///////////////////////////////////////////////////////////////////////////////
Local cTitulo := 'Relat�rio Anexo II - DIEF - Op. de Postos Rev. de Combust�vel'
Local cPerg := '#MYDIEF2P'

CriaSX1(cPerg)

If !Pergunte(cPerg, .T., cTitulo)
	Return Nil
End If

If MV_PAR01 == Nil .or. MV_PAR01 == CTOD('') .or. MV_PAR02 == Nil .or. MV_PAR02 == CTOD('')
	Alert('Informe as datas para gera��o do relat�rio.')
	Return Nil
ElseIf MV_PAR01 > MV_PAR02
	Alert('A data final deve ser maior que a data inicial.')
	Return Nil
EndIf

Processa({|| Executa(cTitulo) },cTitulo,'Aguarde...')

Return Nil

/* ------------------- */

Static Function Executa(cTitulo)

Local cArq := 'MYDIEF2P'
Local cAux, cRet

Private _aExcel := {}

aAdd(_aExcel, {cTitulo})
aAdd(_aExcel, {'Relat�rio emitido em '+DTOC(Date())+' �s '+Time()+' por '+AllTrim(cUsername)})
aAdd(_aExcel, {'Per�odo: '+DTOC(MV_PAR01)+' at� '+DTOC(MV_PAR02)})
aAdd(_aExcel, {''})
aAdd(_aExcel, {'Empresa: '+cEmpAnt+'/'+cFilAnt+'-'+AllTrim(SM0->M0_NOME)+' / '+AllTrim(SM0->M0_FILIAL)})
aAdd(_aExcel, {''})

Secao1() // 1 - Movimenta��o
Secao2() // 2 - Estoque F�sico de Abertura
Secao3() // 3 - Entradas do M�s
Secao4() // 4 - Vendas do M�s
Secao5() // 5 - Informa��es sobre Tanques

cAux := AllTrim(cGetFile('CSV (*.csv)|*.csv', 'Selecione o diret�rio onde ser� salvo o relat�rio', 1, 'C:\', .T., nOR( GETF_LOCALHARD, GETF_LOCALFLOPPY, GETF_NETWORKDRIVE, GETF_RETDIRECTORY ), .F., .T.))
If cAux <> ''
	cAux := SubStr(cAux, 1, RAt('\', cAux)) + cArq
	cAux := cAux + '-' + DTOS(Date()) + '-' + StrTran(Time(), ':', '') + '.csv'
	
	cRet := U_MyArrCsv(_aExcel, cAux, Nil, cTitulo)
	If cRet <> ''
		Alert(cRet)
	EndIf
Else
	Alert('A gera��o do relat�rio foi cancelada!')
EndIf

Return Nil

/* ------------------- */

Static Function Combustivel(cTipo)

Local cRet := ''
Local aComb := {;
	'1 - �lcool Hidratado Comum',;
	'2 - �lcool Hidratado Aditivado',;
	'3 - Diesel Comum',;
	'4 - Diesel Aditivado',;
	'5 - Gasolina Comum',;
	'6 - Gasolina Aditivada';
}
Local nPos := aScan(aComb, {|x| Left(x,1) == cTipo })

If nPos > 0
	cRet := aComb[nPos]
EndIf

Return cRet

/* ------------------- */

Static Function Secao1()

Local cQry := ""
Local nInicial, nFinal, nAfericao, nSemInter, nComInter

cQry := CRLF + " SELECT"
cQry += CRLF + "   LEB_SERIE AS BOMBA"
cQry += CRLF + "  ,L2_TBICO AS BICO"
cQry += CRLF + "  ,B1_TCOMBUS AS COMBUSTIVEL"
cQry += CRLF + "  ,MIN(L2_TENCINI) AS INICIAL"
cQry += CRLF + "  ,MAX(L2_TENCFIM) AS FINAL"
cQry += CRLF + "  ,SUM(CASE"
cQry += CRLF + "     WHEN L1_TAFERIC = 'S' THEN"
cQry += CRLF + "       L2_QUANT"
cQry += CRLF + "     ELSE"
cQry += CRLF + "       0"
cQry += CRLF + "     END"
cQry += CRLF + "   ) AS AFERICAO"
cQry += CRLF + " FROM " + RetSqlName('SL2') + " SL2"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SL1') + " SL1"
cQry += CRLF + " ON  SL1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND L1_FILIAL = '" + xFilial('SL1') + "'"
cQry += CRLF + " AND L1_NUM = L2_NUM"
cQry += CRLF + " AND L1_EMISNF = L2_EMISSAO"
cQry += CRLF + " LEFT JOIN " + RetSqlName('LEB') + " LEB"
cQry += CRLF + " ON  LEB.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND LEB_FILIAL = '" + xFilial('LEB') + "'"
cQry += CRLF + " AND LEB_COD = L2_TBOMBA"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SB1') + " SB1"
cQry += CRLF + " ON  SB1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND B1_FILIAL = '" + xFilial('SB1') + "'"
cQry += CRLF + " AND B1_COD = L2_PRODUTO"
cQry += CRLF + " WHERE SL2.D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND L2_FILIAL = '" + xFilial('SL2') + "'"
cQry += CRLF + "   AND L2_VENDIDO = 'S'"
cQry += CRLF + "   AND LEN(L2_TBICO) > 0"
cQry += CRLF + "   AND L2_TENCINI > 0"
cQry += CRLF + "   AND L2_TENCFIM > 0"
cQry += CRLF + "   AND L2_EMISSAO BETWEEN '" + DTOS(MV_PAR01) + "' AND '" + DTOS(MV_PAR02) + "'"
cQry += CRLF + " GROUP BY"
cQry += CRLF + "   LEB_SERIE"
cQry += CRLF + "  ,L2_TBICO"
cQry += CRLF + "  ,B1_TCOMBUS"
cQry += CRLF + " ORDER BY"
cQry += CRLF + "   L2_TBICO"
cQry += CRLF + "  ,B1_TCOMBUS"

dbUseArea(.T.,'TOPCONN',TCGenQry(,,cQry),'MQRY',.T.)
MQRY->(dbGoTop())

aAdd(_aExcel, {'1 - Movimenta��o'})
aAdd(_aExcel, {'No. de S�rie da Bomba','No. do Bico Abast.','Combust�vel','Inicial','Final','Sem Interven��o','Com Interven��o'})
While !MQRY->(Eof())
	nInicial  := Int(MQRY->INICIAL)
	nFinal    := Int(MQRY->FINAL)
	nAfericao := Int(MQRY->AFERICAO)
	nSemInter := nFinal - nInicial
	nComInter := nSemInter - nAfericao
	aAdd(_aExcel, {AllTrim(MQRY->BOMBA), AllTrim(MQRY->BICO), Combustivel(MQRY->COMBUSTIVEL), nInicial, nFinal, nSemInter, nComInter})
	
	MQRY->(dbSkip())
EndDo
MQRY->(dbCloseArea())
aAdd(_aExcel, {''})

Return Nil

/* ------------------- */

Static Function Secao2()

Local cQry := ""
Local nQtdInicial, nQtdFinal, nPos
Local nTamCod := TamSX3('B1_COD')[1]
Local nTamTq := TamSX3('L2_LOCAL')[1]
Local aSaldo := {}
Local aTanques := {}

cQry := CRLF + " SELECT"
cQry += CRLF + "   LET_NUMERO AS TANQUE"
cQry += CRLF + "  ,LET_PRODUT AS PRODUTO"
cQry += CRLF + "  ,B1_TCOMBUS AS COMBUSTIVEL"
cQry += CRLF + " FROM " + RetSqlName('LET') + " LET"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SB1') + " SB1"
cQry += CRLF + " ON  SB1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND B1_FILIAL = '" + xFilial('SB1') + "'"
cQry += CRLF + " AND B1_COD = LET_PRODUT"
cQry += CRLF + " WHERE LET.D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND LET_FILIAL = '" + xFilial('LET') + "'"
cQry += CRLF + " GROUP BY"
cQry += CRLF + "   LET_NUMERO"
cQry += CRLF + "  ,LET_PRODUT"
cQry += CRLF + "  ,B1_TCOMBUS"
cQry += CRLF + " ORDER BY"
cQry += CRLF + "   LET_NUMERO"
cQry += CRLF + "  ,LET_PRODUT"
cQry += CRLF + "  ,B1_TCOMBUS"

dbUseArea(.T.,'TOPCONN',TCGenQry(,,cQry),'MQRY',.T.)
MQRY->(dbGoTop())

While !MQRY->(Eof())
	aSaldo      := CalcEst(PadL(MQRY->PRODUTO,nTamCod),PadL(MQRY->TANQUE,nTamTq),MV_PAR01)
	nQtdInicial := aSaldo[1]
	
	aSaldo      := CalcEst(PadL(MQRY->PRODUTO,nTamCod),PadL(MQRY->TANQUE,nTamTq),MV_PAR02+1)
	nQtdFinal   := aSaldo[1]
	
	nPos := aScan(aTanques, {|x| x[1] == AllTrim(MQRY->TANQUE) .and. x[2] == MQRY->COMBUSTIVEL })
	If nPos > 0
		aTanques[nPos][3] += nQtdInicial
		aTanques[nPos][4] += nQtdFinal
	Else
		aAdd(aTanques, {AllTrim(MQRY->TANQUE), MQRY->COMBUSTIVEL, nQtdInicial, nQtdFinal})
	EndIf
	
	MQRY->(dbSkip())
EndDo
MQRY->(dbCloseArea())

aAdd(_aExcel, {'2 - Estoque F�sico de Abertura'})
aAdd(_aExcel, {'Tanque','Combust�vel','Quantidade Inicial','Quantidade Final'})
nTam := Len(aTanques)
For nI := 1 to nTam
	aAdd(_aExcel, {aTanques[nI][1], Combustivel(aTanques[nI][2]), Int(aTanques[nI][3]), Int(aTanques[nI][4])})
Next nI
aAdd(_aExcel, {''})

Return Nil

/* ------------------- */

Static Function Secao3()

Local cQry := ""

cQry := CRLF + " SELECT"
cQry += CRLF + "   A2_CGC AS CNPJ"
cQry += CRLF + "  ,B1_TCOMBUS AS COMBUSTIVEL"
cQry += CRLF + "  ,D1_DOC AS NOTAFISCAL"
cQry += CRLF + "  ,D1_EMISSAO AS EMISSAO"
cQry += CRLF + "  ,D1_LOCAL AS TANQUE"
cQry += CRLF + "  ,SUM(D1_QUANT) AS QUANTIDADE"
cQry += CRLF + "  ,SUM(D1_VUNIT)/COUNT(D1_VUNIT) AS PRECO"
cQry += CRLF + "  ,F1_VALBRUT AS VALORTOTAL"
cQry += CRLF + " FROM " + RetSqlName('SD1') + " SD1"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SF1') + " SF1"
cQry += CRLF + " ON  SF1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND F1_FILIAL = '" + xFilial('SF1') + "'"
cQry += CRLF + " AND F1_DOC = D1_DOC"
cQry += CRLF + " AND F1_SERIE = D1_SERIE"
cQry += CRLF + " AND F1_FORNECE = D1_FORNECE"
cQry += CRLF + " AND F1_LOJA = D1_LOJA"
cQry += CRLF + " AND F1_EMISSAO = D1_EMISSAO"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SA2') + " SA2"
cQry += CRLF + " ON  SA2.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND A2_FILIAL = '" + xFilial('SA2') + "'"
cQry += CRLF + " AND A2_COD = D1_FORNECE"
cQry += CRLF + " AND A2_LOJA = D1_LOJA"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SB1') + " SB1"
cQry += CRLF + " ON  SB1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND B1_FILIAL = '" + xFilial('SB1') + "'"
cQry += CRLF + " AND B1_COD = D1_COD"
cQry += CRLF + " WHERE SD1.D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND D1_FILIAL = '" + xFilial('SD1') + "'"
cQry += CRLF + "   AND D1_DTDIGIT BETWEEN '" + DTOS(MV_PAR01) + "' AND '" + DTOS(MV_PAR02) + "'"
cQry += CRLF + "   AND D1_TIPO = 'N'"
cQry += CRLF + "   AND D1_COD IN ("
cQry += CRLF + "     SELECT"
cQry += CRLF + "        LEI_PROD"
cQry += CRLF + "     FROM " + RetSqlName('LEI') + " LEI"
cQry += CRLF + "     WHERE LEI.D_E_L_E_T_ <> '*'"
cQry += CRLF + "       AND LEI_FILIAL = '" + xFilial('LEI') + "'"
cQry += CRLF + "   )"
cQry += CRLF + " GROUP BY"
cQry += CRLF + "   A2_CGC"
cQry += CRLF + "  ,B1_TCOMBUS"
cQry += CRLF + "  ,D1_DOC"
cQry += CRLF + "  ,D1_EMISSAO"
cQry += CRLF + "  ,D1_LOCAL"
cQry += CRLF + "  ,F1_VALBRUT"
cQry += CRLF + " ORDER BY"
cQry += CRLF + "   A2_CGC"
cQry += CRLF + "  ,B1_TCOMBUS"
cQry += CRLF + "  ,D1_DOC"
cQry += CRLF + "  ,D1_EMISSAO"
cQry += CRLF + "  ,D1_LOCAL"

dbUseArea(.T.,'TOPCONN',TCGenQry(,,cQry),'MQRY',.T.)
MQRY->(dbGoTop())

aAdd(_aExcel, {'3 - Entradas do M�s'})
aAdd(_aExcel, {'CNPJ Remetente','Combust�vel','No. Nota Fiscal','Data Nota Fiscal','N�mero Tanque Descarga','Quantidade (Litros)','Pre�o (Litros)','Valor Total da Nota Fiscal'})
While !MQRY->(Eof())
	aAdd(_aExcel, {AllTrim(MQRY->CNPJ), Combustivel(MQRY->COMBUSTIVEL), AllTrim(MQRY->NOTAFISCAL), STOD(MQRY->EMISSAO), AllTrim(MQRY->TANQUE), MQRY->QUANTIDADE, MQRY->PRECO, MQRY->VALORTOTAL})
	
	MQRY->(dbSkip())
EndDo
MQRY->(dbCloseArea())
aAdd(_aExcel, {''})

Return Nil

/* ------------------- */

Static Function Secao4()

Local cQry := ""

cQry := CRLF + " SELECT"
cQry += CRLF + "   B1_TCOMBUS AS COMBUSTIVEL"
cQry += CRLF + "  ,SUM(L2_VLRITEM) AS VALOR"
cQry += CRLF + "  ,SUM(L2_VALDESC) AS DESCONTO"
cQry += CRLF + "  ,SUM(L2_VALACRS) AS ACRESCIMO"
cQry += CRLF + "  ,SUM(L2_QUANT) AS QUANTIDADE"
cQry += CRLF + " FROM " + RetSqlName('SL2') + " SL2"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SL1') + " SL1"
cQry += CRLF + " ON  SL1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND L1_FILIAL = '" + xFilial('SL1') + "'"
cQry += CRLF + " AND L1_NUM = L2_NUM"
cQry += CRLF + " AND L1_EMISNF = L2_EMISSAO"
cQry += CRLF + " LEFT JOIN " + RetSqlName('SB1') + " SB1"
cQry += CRLF + " ON  SB1.D_E_L_E_T_ <> '*'"
cQry += CRLF + " AND B1_FILIAL = '" + xFilial('SB1') + "'"
cQry += CRLF + " AND B1_COD = L2_PRODUTO"
cQry += CRLF + " WHERE SL2.D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND L2_FILIAL = '" + xFilial('SL2') + "'"
cQry += CRLF + "   AND L2_VENDIDO = 'S'"
cQry += CRLF + "   AND L1_TAFERIC <> 'S'"
cQry += CRLF + "   AND L2_EMISSAO BETWEEN '" + DTOS(MV_PAR01) + "' AND '" + DTOS(MV_PAR02) + "'"
cQry += CRLF + " GROUP BY"
cQry += CRLF + "  B1_TCOMBUS"
cQry += CRLF + " ORDER BY"
cQry += CRLF + "  B1_TCOMBUS"

dbUseArea(.T.,'TOPCONN',TCGenQry(,,cQry),'MQRY',.T.)
MQRY->(dbGoTop())

aAdd(_aExcel, {'4 - Vendas do M�s'})
aAdd(_aExcel, {'Combust�vel','Valor Cont�bil da Vendas','Quantidade Total'})
While !MQRY->(Eof())
	aAdd(_aExcel, {Combustivel(MQRY->COMBUSTIVEL), Round(MQRY->VALOR + MQRY->DESCONTO - MQRY->ACRESCIMO,2), Int(MQRY->QUANTIDADE)})
	
	MQRY->(dbSkip())
EndDo
MQRY->(dbCloseArea())
aAdd(_aExcel, {''})

Return Nil

/* ------------------- */

Static Function Secao5()

Local cQry := ""

cQry := CRLF + " SELECT"
cQry += CRLF + "  LET_NUMERO AS TANQUE"
cQry += CRLF + " ,LET_CAPACI AS CAPACIDADE"
cQry += CRLF + " ,(SELECT"
cQry += CRLF + "      COUNT(*)"
cQry += CRLF + "   FROM ("
cQry += CRLF + "     SELECT"
cQry += CRLF + "        L2_TBICO"
cQry += CRLF + "     FROM " + RetSqlName('SL2') + " SL2"
cQry += CRLF + "     LEFT JOIN " + RetSqlName('LEI') + " LEI"
cQry += CRLF + "     ON  LEI.D_E_L_E_T_ <> '*'"
cQry += CRLF + "     AND LEI_FILIAL = '" + xFilial('LEI') + "'"
cQry += CRLF + "     AND LEI_BICO = L2_TBICO"
cQry += CRLF + "     WHERE SL2.D_E_L_E_T_ <> '*'"
cQry += CRLF + "       AND L2_FILIAL = '" + xFilial('SL2') + "'"
cQry += CRLF + "       AND L2_VENDIDO = 'S'"
cQry += CRLF + "       AND L2_EMISSAO BETWEEN '" + DTOS(MV_PAR01) + "' AND '" + DTOS(MV_PAR02) + "'"
cQry += CRLF + "       AND LEI_TANQUE = LET_NUMERO"
cQry += CRLF + "     GROUP BY"
cQry += CRLF + "        L2_TBICO"
cQry += CRLF + "     ) AS QTD_BICOS"
cQry += CRLF + "   ) AS BICOS"
cQry += CRLF + " FROM " + RetSqlName('LET') + " LET"
cQry += CRLF + " WHERE LET.D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND LET_FILIAL = '" + xFilial('LET') + "'"
cQry += CRLF + "   AND LET_DTDESA < '" + DTOS(MV_PAR01) + "'"
cQry += CRLF + " ORDER BY"
cQry += CRLF + "  LET_NUMERO"

dbUseArea(.T.,'TOPCONN',TCGenQry(,,cQry),'MQRY',.T.)
MQRY->(dbGoTop())

aAdd(_aExcel, {'5 - Informa��es sobre Tanques'})
aAdd(_aExcel, {'Tanques','Capacidade Nominal (Litros)','No. de Bicos no In�cio do M�s','No. de Bicos no Final do M�s'})
While !MQRY->(Eof())
	aAdd(_aExcel, {AllTrim(MQRY->TANQUE), MQRY->CAPACIDADE, MQRY->BICOS, MQRY->BICOS})
	
	MQRY->(dbSkip())
EndDo
MQRY->(dbCloseArea())
aAdd(_aExcel, {''})

Return Nil

/* ------------------- */

Static Function CriaSX1(cPerg,cCFPad)

Local nTamGrp := Len(SX1->X1_GRUPO)
Local aHelpPor := {}, aHelpEng := {}, aHelpSpa := {}
Local cNome

aHelpPor := {}
aAdd(aHelpPor, 'Informe a data inicial/final para    ')
aAdd(aHelpPor, 'gera��o do relat�rio.                ')
cNome := 'Data inicial'
PutSx1(PadR(cPerg,nTamGrp), '01', cNome, cNome, cNome,;
'MV_CH1', 'D', 8, 0, 0, 'G', '', '', '', '', 'MV_PAR01',;
'', '', '', '',;
'', '', '',;
'', '', '',;
'', '', '',;
'', '', '',;
aClone(aHelpPor), aClone(aHelpEng), aClone(aHelpSpa))

cNome := 'Data final'
PutSx1(PadR(cPerg,nTamGrp), '02', cNome, cNome, cNome,;
'MV_CH2', 'D', 8, 0, 0, 'G', '', '', '', '', 'MV_PAR02',;
'', '', '', '',;
'', '', '',;
'', '', '',;
'', '', '',;
'', '', '',;
aClone(aHelpPor), aClone(aHelpEng), aClone(aHelpSpa))

Return Nil
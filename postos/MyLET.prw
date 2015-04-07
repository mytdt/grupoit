#include "rwmake.ch"
#include "protheus.ch"

///////////////////////////////////////////////////////////////////////////////
User Function MyLET()
///////////////////////////////////////////////////////////////////////////////
// Data : 02/04/2015
// User : Thieres Tembra
// Desc : Manipula‹o da tabela LET
// A‹o : Controla o cadastro de tanques
///////////////////////////////////////////////////////////////////////////////

Local aCores := {}

Private cCadastro := 'Cadastro de Tanques de Combustíveis - LET'
Private aRotina := { {'Pesquisar' ,'AxPesqui'  ,0,01} ,;
					 {'Visualizar','AxVisual'  ,0,02} ,;
					 {'Incluir'   ,'U_MyLET001',0,03} ,;
					 {'Alterar'   ,'U_MyLET001',0,04} ,;
					 {'Excluir'   ,'U_MyLET001',0,05} ,;
					 {'Desativar' ,'U_MyLET001',0,10} ,;
					 {'Legenda'   ,'U_MyLET001',0,11} }
Private cDelFunc := '.T.'
Private cString := 'LET'


aAdd(aCores, {"AllTrim(DTOS(LET->LET_DTDESA)) == ''", 'BR_VERDE'})
aAdd(aCores, {"AllTrim(DTOS(LET->LET_DTDESA)) <> ''", 'BR_VERMELHO'})

dbSelectArea(cString)
dbSetOrder(1)
mBrowse(6,1,22,75,cString,,,,,,aCores)

//Desativa F12
Set Key 123 To

Return Nil

/* ------------------------ */

User Function MyLET001(cAlias, nRecNo, nOpc)

Local nOpcA := 0
Local aParam := {}
Local aLegenda := {}
Local cBicos := ''
Local cTanque := ''

Private _aButtons := {}
Private _aCpos := {}
Private _nMyOpc := nOpc

//adiciona codeblock a ser executado no inicio, meio e fim
aAdd( aParam,  {|| .T. })  //antes da abertura
aAdd( aParam,  {|| U_MyLET002() })  //ao clicar no botao ok
aAdd( aParam,  {|| .T. })  //durante a transacao
aAdd( aParam,  {|| .T. })  //termino da transacao

dbSelectArea(cAlias)
If nOpc == 3
	//AxInclui( cAlias, nReg, nOpc, aAcho, cFunc, aCpos, cTudoOk, lF3, cTransact, aButtons, aParam, aAuto, lVirtual, lMaximized, cTela, lPanelFin, oFather, aDim, uArea)
	nOpcA := AxInclui(cAlias,nRecNo,3,,,,'.T.',.F.,,_aButtons,aParam,,,.T.,,,,,)
ElseIf nOpc == 4
	//campos que podem ser alterados
	_aCpos := {'LET_PRODUT', 'LET_CAPACI', 'LET_LASTRO'}
	//AxAltera(cAlias,nReg,nOpc,aAcho,aCpos,nColMens,cMensagem,cTudoOk,cTransact,cFunc, aButtons, aParam, aAuto, lVirtual, lMaximized, cTela,lPanelFin,oFather,aDim,uArea)
	nOpcA := AxAltera(cAlias,nRecNo,4,,_aCpos,,,'.T.',,,_aButtons,aParam,,,.T.,,,,,)
ElseIf nOpc == 5
	cTanque := LET->LET_NUMERO
	cBicos := BicoTanque(cTanque)
	If cBicos <> ''
		Alert('Os bicos ' + cBicos + ' estão ligados ao tanque ' +;
		AllTrim(cTanque) + '. Não será possível excluir este tanque. ' +;
		'Considere desativá-lo.')
	Else
		//campos que serão exibidos
		_aCpos := {'LET_NUMERO', 'LET_PRODUT', 'LET_DESCRI', 'LET_CAPACI', 'LET_LASTRO', 'LET_DTATIV', 'LET_DTDESA'}
		//AxDeleta(cAlias,nReg,nOpc,cTransact,aCpos,aButtons,aParam,aAuto,lMaximized,cTela,aAcho,lPanelFin,oFather,aDim)
		nOpcA := AxDeleta(cAlias,nRecNo,5,,_aCpos,_aButtons,aParam,,.T.,,,,,)
	EndIf
ElseIf nOpc == 10
	//desativar tanque
	cTanque := LET->LET_NUMERO
	cBicos := BicoTanque(cTanque)
	If cBicos <> ''
		cBicos := 'Os bicos ' + cBicos + ' que estão ligados a este tanque também serão desativados.'
	EndIf
	If MsgYesNo('Deseja realmente desativar o tanque ' + AllTrim(cTanque) +;
	' na data de ' + DTOC(dDatabase) + '?' +;
	Iif(cBicos <> '',CRLF + CRLF + cBicos,''))
		RecLock('LET', .F.)
		LET->LET_DTDESA := dDatabase
		LET->(MsUnlock())

		//TODO: verificar desativação dos bicos
	EndIf
ElseIf nOpc == 11
	//legenda
	aAdd(aLegenda, {'BR_VERDE'   , 'Ativo'})
	aAdd(aLegenda, {'BR_VERMELHO', 'Desativado'})
	BrwLegenda(cCadastro, 'Legenda', aLegenda)
EndIf

Return nOpcA

/* ------------------------ */

User Function MyLET002()

Local lOk      := .T.
Local cTanque  := M->LET_NUMERO
Local cProduto := M->LET_PRODUT
Local cDescric := M->LET_DESCRI
Local dAtiva   := M->LET_DTATIV
Local cBicos   := ''

If _nMyOpc == 3
	If !MsgYesNo('O tanque ' + AllTrim(cTanque) + ' será criado com data de ativação em ' +;
	DTOC(dAtiva) + ' e com o produto ' + AllTrim(cProduto) + '-' + AllTrim(cDescric) +;
	'. Confirma a operação?')
		lOk := .F.
	EndIf
ElseIf _nMyOpc == 4
	If LET->LET_PRODUT <> M->LET_PRODUT
		cBicos := BicoTanque(cTanque)
		If cBicos <> ''
			If !MsgYesNo('!!! CUIDADO !!!' + CRLF + CRLF +;
			'Você trocou o produto do tanque ' + AllTrim(cTanque) + '.' + CRLF +;
			'Os bicos ' + cBicos + ' estão ligados a este tanque e ' +;
			'seus produtos também serão modificados.' + CRLF + CRLF +;
			'Deseja continuar?')
				lOk := .F.
			Else
				//TODO: trocar produtos na LEI
			EndIf
		EndIf
	EndIf
EndIf

Return lOk

/* ------------------------ */

Static Function BicoTanque(cTanque)

Local cQry := ''
Local cBicos := ''

cQry := CRLF + " SELECT"
cQry += CRLF + "    LEI_BICO"
cQry += CRLF + " FROM " + RetSqlName('LEI')
cQry += CRLF + " WHERE D_E_L_E_T_ <> '*'"
cQry += CRLF + "   AND LEI_FILIAL = '" + xFilial('LEI') + "'"
cQry += CRLF + "   AND LEI_TANQUE = '" + cTanque + "'"

dbUseArea(.T.,'TOPCONN',TCGenQry(,,cQry),'MQRY',.T.)

MQRY->(dbGoTop())
While !MQRY->(Eof())
	cBicos += MQRY->LEI_BICO + ', '
	MQRY->(dbSkip())
EndDo

MQRY->(dbCloseArea())

If cBicos <> ''
	cBicos := Left(cBicos, Len(cBicos)-2)
EndIf

Return cBicos
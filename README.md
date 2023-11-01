# CrossCut
Primeiro projeto realizado no âmbito da disciplina de PFL.

### Desenvolvido pelo Grupo Crosscut_1 da Turma 01
- Isabel Maria Lima Moutinho (up202108767)
- Tiago Ribeiro de Sá Cruz (up202108810)

## Correr o Código
- Instalação de SICStus Prolog 4.8
- Consultar o código em src/proj.pl
- Executar o comando ```play.```

## Descrição do Jogo

O crosscut joga-se num tabuleiro quadrado, pelo menos 5x5 e menos de 10x10. Os dois jogadores azul e vermelho jogam á vez colocando discos no tabuleiro, um disco por turno, começando com o vermelho. É obrigatorio jogar. Se não houverem jogadas válidas disponíveis, o seu turno é passado à frente. <br>
O objetivo do jogo é formar um segmento (horizontal ou vertical) de um lado ao outro, sem contar com as bordas. <br>
Um jogador poderá colocar uma peça em todos os locais do tabuleiro, menos nas bordas, a não ser que consiga fazer um 'flip'. Um 'flip' consiste em, quando um jogador coloca as suas peças nos cantos de peças do adversário, então as peças do adversário tornam-se suas, a não ser que, o segmento flanqueado do adversário seja maior ou igual ao novo segmento do jogador. Caso este 'flip' legal seja atingido, um jogador poderá colocar a sua peça na borda temporariamente, de modo a fazer o 'flip', mas de seguida essa mesma peça será retirada.

[Regras Oficiais](http://marksteeregames.com/Crosscut_rules.pdf)

## Game Logic

### Representação interna do Game State

### Visualização do Jogo

### Validação e Execução das jogadas

### Lista de jogadas válidas

### Fim de Jogo

### Evaluação do estado do jogo

### Jogadas do Computador

## Conclusões

## Bibliografia

- GitHub Co-Pilot
- ~~ChatGPT~~ (waste of time)
- Professor Gonçalo Leão
- https://www.swi-prolog.org/
- https://sicstus.sics.se/documentation.html
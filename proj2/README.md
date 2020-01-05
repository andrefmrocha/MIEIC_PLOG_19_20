Para executar o projeto é necessário abrir uma janela do terminal de SICSTUS na pasta src.

Para resolver um puzzle, é possível utilizar o predicado close_or_far_display(+Board).

Para a geração de um puzzle é necessário utilizar o predicado generate(+Side, +Unique, +Name) onde:
    - Side é o valor para o lado do puzzle, 
    - Unique é um valor entre unique1, unique2 e no, onde unique1 e unique2 representa duas diferentes estratégias para gerar um board de solução única
    - Name é o nome que o predicado deve ser guardado no ficheiro "generated_boards.pl"
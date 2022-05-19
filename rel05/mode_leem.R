# implementacao inicial do calculo de moda para o pacote leem

# o ideal seria a chamar de "mode" mas o R já tem uma função assim. Fica como "most frequent" por enquanto

mfreq <- function(x, ...) {
  
  # fazendo a proteção para uso no pacote e classe leem. Apenas objetos da classe por enquanto. Ainda há de implementar o default
  
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE)
  
  # forcando ao objeto a ter também o nódulo de "tabfreq" caso ela não tenha. Necessario para implementação desta funcao
  
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  
  # O leem trabalha com três tipos de dados até agora: quantitativos discretos, qualitativos e quantitativos contínuos
  # A moda se dá diferente de maneira difierente em cada uma delas. A comecar com a qualitativa:
  # o primeiro condicional detecta se o objeto leem é discreto
  
  if (attr(x, "variable") == "discrete") {
    
    # variável auxiliar numchar para armazenar o valor se há caracteres dentro dos dados brutos do objeto
    
    numchar <- is.numeric(x$estat$raw_data)
    
    # 1 significa que só há numérico. 0 é que há caracteres. Se ativar esta condicional, será calculada a moda dos dados qualitativos
    
    if (numchar == 0) {
      
      # which.max procura a posição de um vetor que possui o maior valor. Será escaneado a coluna de frequências e "pos" armazenará esta posição 
      
      pos <- which.max(x$tabela$Fi)
      
      # buscando o nome que está na mesma posição de frequência mais alta, ou seja, a classe que será a moda
      
      mo <- x$tabela$Groups[pos]
    
    # este "else" já executará para variáveis discretas quantitativas
    
    } else {
    
    # extraindo os números brutos do objeto e armazenando a variável auxiliar "tabela" 
    
    tabela <- table(x$estat$raw_data)
    
    # resgatando o nome da maior posição novamente
    
    pos <- which.max(tabela)
    
    # como "which.max" retorna em nome, deve-se assegurar que a resposta não saia como caractere. "as.numeric" garante isso para números
    
    mo <- as.numeric(names(tabela[pos]))
    }
  }
  
  # eu havia deixado esta parte da função fora de uma condicional mas acaba que, no caso de variáveis qualitativas e discretas entram no cálculo da contínua e saem desconfiguradas
  
  if (attr(x, "variable") == "continuous") {
 
  # procurando pela posição da classe de maior frequência 
 
  pos <- which.max(x$tabela$Fi)
  
  # a fórmula da moda para dados contínuos é: Limite inferior da classe + (delta1/(delta1 - delta2))*amplitude
  # delta1: frequencia da moda - frequencia da classe anterior
  # delta2:     "       "   "  - frequência da próxima classe
  # para delta 1, encontramos o probçema quando for a moda for a primeira classe. Como não há classe anterior, não haverá frequência da classe anterior
  # para isso, separamos uma condicional para detectar este caso e colocar tal frequência como zero
  
  
  if (pos == 1) {
    aux1 <- 0
  } else{
    aux1 <- x$tabela$Fi[pos - 1]
  }
  
  # Mesma coisa para delta 2 e a última classe. Como o número de classes dobra como a última posição da última classe, usamos este dado para completar a condicional
  
  if(pos == x$estat$Numero_de_classes){
  
    aux2 <- 0
  } else{
    aux2 <- x$tabela$Fi[pos + 1]
  }
  
  # terminando o cálculo
  
  del1 <- x$tabela$Fi[pos] - aux1
  del2 <- x$tabela$Fi[pos] - aux2
  mo <- x$estat$LI_classes[pos] + (del1 / (del1 + del2)) * x$estat$Ampl_clas
  }
  
  # Imprimindo o resultado
  
  return(mo)
}


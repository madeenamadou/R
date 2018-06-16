# Notes sur les étapes 

### Importation de la base

### Catégorisation des variables en actives et illustratives

### Calculs de quelques statistiques descriptives

### Exécution de l&#39;ACP

### Calculs des Écarts-type, pourcentages et pourcentages cumulés associés aux composantes

« Écarts-type » : ce sont les racines carré des valeurs propres associées.

« Pourcentages » associés aux différentes composantes. Ils permettent d&#39;apprécier la quantité d&#39;information apportée par chacun 
des axes.

« Pourcentages cumulés » associés aux composantes. On peut observer par exemple que les 02 premiers axes apportent 58,85 % de l&#39;information.

### Calcul des 5 premières valeurs propres ou variances respectives associées aux 5 premiers axes

### Intervalle laplacien d&#39;Anderson (au seuil de 95%)

Ces intervalles permettent d&#39;apprécier la qualité de l&#39;échantillon retenu pour l&#39;étude. Il s&#39;agit de vérifier si pour les composantes retenues, les valeurs propres associées sont plus ou moins bien centrées dans leur intervalle de confiance. Dans le cas contraire, un reéchantillonnage peut permettre de réduire les intervalles.

### Histogramme des 5 premières valeurs propres

Les sommets ont été joints afin d&#39;illustrer la chute lors du passage d&#39;une valeur propre à une autre.  Selon « la règle du coude », les axes retenus pour l&#39;analyse sont ceux avant lesquels la chute devient brusque, avec une pente très raide. Ici nous retiendrons les deux premiers.

Un autre critère, la règle de Kaiser demande de retenir plutôt les axes pour lesquels les valeurs propres associés sont supérieures à 1.

### Coordonnées des variables sur les 5 premiers axes

Les coordonnées calculées ci-dessus nous permettrons d&#39;apprécier la proximité des variables par rapport au cercle de corrélation. Nous retiendrons donc pour l&#39;analyse des composantes principales, les variables qui auraient les coordonnées les plus élevées en valeur absolue pour un axe et pour l&#39;autre.

Nous pouvons observer que les variables SOLD VADD DEPO sont les plus pertinentes par rapport à l&#39;axe 1, et les variables RETR MEMP NBPR pour l&#39;axe 2.

Le signe des coordonnées précise le côté duquel se trouve la variable (côté positif pour (+) et négatif pour (-).

### Cosinus carré des variables sur les 5 premiers axes

Les cosinus carrés calculés ci-dessus nous permettrons d&#39;apprécier la proximité des variables par rapport à chacun des axes factoriels. Plus grand est le cosinus carré d&#39;une variable, plus proche elle est de l&#39;axe factoriel associé. Nous allons retenir pour l&#39;analyse, les variables qui auraient les cosinus carrés les plus grands respectivement pour nos 02 composantes.

Nous pouvons observer que les variables SOLD VADD DEPO sont les plus près de l&#39;axe 1 et les variables RETR MEMP NBPR les plus près de l&#39;axe 2.

### Contribution des variables à l&#39;inertie des 5 premiers axes

### Coordonnées des individus avec les 5 premières composantes

### Calcul des disto des individus

Le disto traduit l&#39;éloignement de l&#39;individu par rapport à l&#39;origine du repère. Les individus les plus éloignés (disto plus grands) sont considérés atypiques.

### Calcul des cosinus carré des individus

Le cosinus carré traduit la proximité à l&#39;axe factoriel associé. Un individu bien représenté est aussi proche que possible de l&#39;axe.

### Contributions des individus à la formation des 5 premières composantes

La contribution résume les deux informations précédentes. Elle révèle ceux qui contribuent le mieux à la formation de l&#39;axe associé.

### Nuages des variables

Pour interpréter le nuage des variables, nous devons rechercher 02 choses. La proximité des variables par rapport à chacun des 02 axes que nous avons retenu et à la proximité des variables par rapport au cercle de corrélation.  Les variables les plus proches d&#39;un axe et du cercle sont les mieux représentées sur cet axe et deviennent utile à l&#39;interprétation.

### Nuage des individus

### Nuage individus x variables

Attention : il ne faut pas chercher une quelconque proximité entre les individus et les variables dans ce graphique. Ce sont les directions qui sont importantes. Des directions opposées signifient que ces variables opposent deux groupes d&#39;individus.

### Extras : Recodage des variables continues illustratives TAIL et AGEC (transformation en classes de valeurs)

Les variables TAIL AGEC sont continues, mais rangées dans le groupe des illustratives. Lorsque nous les intégrons dans le nuage des variables, elles apparaissent mal représentées. Pour palier cela, il faut les recoder. Nous allons créer deux nouvelles variables TAILR et AGECR, nominales. Il s&#39;agit de regrouper les anciennes modalités en classes. Les classes deviennent les modalités des variables créées.

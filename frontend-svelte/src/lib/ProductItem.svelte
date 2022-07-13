<script lang="ts">
  import {last} from 'lodash'

  export let product: any

  const lastList = (prices: {list: number; special: number}[]) =>
    last(prices)?.list || 0
  const logoImg = (source: string) => {
    switch (source) {
      case 'cellphones.com.vn':
        return 'https://cdn2.cellphones.com.vn/200x/media/favicon/default/logo-cps.png'
      case 'thegioididong.com':
        return 'https://www.thegioididong.com/favicon_TGDD.ico'
      case 'fptshop.com.vn':
        return 'https://secure.gravatar.com/avatar/8bda94c2ebc81a48bf241aacfc31000a?s=96&d=mm&r=g'
      default:
        return ''
    }
  }
  const fmtPrice = new Intl.NumberFormat(`vi-VN`, {
    currency: `VND`,
    style: 'currency',
  })

  const emptyImg =
    'https://www.pngkey.com/png/full/233-2332677_image-500580-placeholder-transparent.png'
</script>

<div class="max-w-xs mx-auto">
  <div>
    <a href={product.detail.link} target="_blank">
      <div
        class="w-48 h-48 rounded bg-contain bg-center bg-no-repeat"
        style={`background-image: url("${product.detail.image || emptyImg}")`}
      />
    </a>
  </div>
  <div>{product.detail.name}</div>
  <div>
    {#each product.detail.moreInfo as m}
      <div class="badge badge-outline">{m}</div>
    {/each}
  </div>
  <div class="flex items-center">
    <img src={logoImg(product.detail.source)} alt="0" class="w-5 h-5 mr-2" />
    {product.detail.source}
  </div>
  <div>
    <div>
      <span class="text-red-400">
        List Price: {fmtPrice.format(lastList(product.prices))}
      </span>
      <span class="text-red-200">
        {product.reduction.toFixed(2)}%
      </span>
    </div>
    {#each product.prices as price, i}
      <div>
        <span>Date: {price.date}</span>
        <span>-</span>
        <span class={i === 0 ? 'text-blue-400' : ''}>
          {fmtPrice.format(price.special)}
        </span>
      </div>
    {/each}
  </div>
</div>

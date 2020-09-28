import rasterio
import numpy as np

ds = rasterio.open('./data/hydrosheds_direction.tif','r')
mfd=ds.read(1) # maximum flow direction
id=np.reshape(np.arange(0,mfd.size),mfd.shape)
flows_to64=np.reshape(np.arange(0,mfd.size),mfd.shape)
flows_to=flows_to64.astype('int32')

def move_to(mfd,i,j):

    # drainage direction
    # 3 2 1
    # 4 * 8
    # 5 6 7

    switcher = {
        1: (i-1,j+1),
        2: (i-1,j),
        3: (i-1,j-1),
        4: (i,j-1),
        5: (i+1,j-1),
        6: (i+1,j),
        7: (i+1,j+1),
        8: (i,j+1)
    }
    return switcher.get(mfd, -1)

for i in range(0, mfd.shape[0]):
    for j in range(0, mfd.shape[1]):
        if mfd[i,j]<1:
            flows_to[i,j]=-1
        else:
            index=move_to(mfd[i,j],i,j)
            flows_to[i,j]=id[index[0],index[1]]


flows_to_dataset = rasterio.open(
    './data/flows_to.tif',
    'w',
    driver='GTiff',
    height=flows_to.shape[0],
    width=flows_to.shape[1],
    count=1,
    dtype=rasterio.int32,
    crs=ds.crs,
    transform=ds.transform,
    )
flows_to_dataset.write(flows_to,1)
flows_to_dataset.close()

id_matrix_dataset = rasterio.open(
    './data/flows_from.tif',
    'w',
    driver='GTiff',
    height=id.shape[0],
    width=id.shape[1],
    count=1,
    dtype=rasterio.int32,
    crs=ds.crs,
    transform=ds.transform,
    )
id_matrix_dataset.write(id.astype('int32'),1)
id_matrix_dataset.close()



ds.close()
